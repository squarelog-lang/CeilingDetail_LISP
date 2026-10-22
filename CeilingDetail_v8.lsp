;;; ==========================================================================
;;; 건축 경량철골천정틀 단면 자동 작도 LISP (v8 - 최종 완성본)
;;; 명령어: CCD
;;; 인코딩: CP949 (ANSI / 한글 Windows AutoCAD 완벽 호환)
;;; ==========================================================================
;;; [v8 주요 업데이트 사항]
;;; 1. 스페이스바(Enter) 입력 시 즉시 옵션 대화상자 호출 (O 입력도 호환)
;;;    - CCD 실행 후 바로 마우스 클릭 시: 3점(또는 2점) 즉시 작도
;;;    - CCD 실행 후 스페이스바(Enter) 입력 시: 옵션 대화상자 열기
;;; 2. T-BAR 및 CLIP-BAR 클립 형상을 첨부 도면(우측 형상)과 100% 동일하게 변경
;;;    - T-BAR: 마감판 내부 수직 분할선 제거, 하부 역T자 플랜지 상단을 마감판 밑면(yb)에 일치
;;;    - CLIP-BAR: 상단 사선 걸림 와이어 + 하부 역삼각형 이중 X자 집게 다리 & 중앙 심재 구현
;;; 3. 즐겨찾기 [현재 세팅 저장] 클릭 시 좌측 버튼 라벨이 즉시 갱신되도록 DCL 재생성 루프 적용
;;;    (AutoCAD DCL은 이미 열린 버튼의 label을 set_tile로 바꾸지 못하는 한계가 있어,
;;;     저장 클릭 시 DCL 파일의 버튼 label 자체를 새 텍스트로 교체하여 즉시 재표시함)
;;; ==========================================================================

(defun c:CCD (/ *error* os cm cl
                inp mode_val pt1 pt2 pt3 p1 p2 y_base h_dyn_len
                lay_list lay_str idx_cb idx_hb idx_fin
                lay_cb lay_hb lay_fin
                dcl_file fp dcl_id dcl_status
                cb_opt cb_w cb_d
                bar_type fin_type custom_t
                h_len h_pitch
                c_p1 c_p2 bar_x1 bar_x2 ceil_w
                fin_t1 fin_t2 fin_total
                hbar_y1 hbar_y2
                num_h span_h start_h cur_x)

  (vl-load-com)

  ;; 에러 처리 함수
  (defun *error* (msg)
    (if os (setvar "OSMODE" os))
    (if cm (setvar "CMDECHO" cm))
    (if cl (setvar "CLAYER" cl))
    (if (and dcl_file (findfile dcl_file)) (vl-file-delete dcl_file))
    (if (not (wcmatch (strcase msg t) "*break*,*cancel*,*exit*"))
      (princ (strcat "\n[오류] " msg))
    )
    (princ)
  )

  (setq os (getvar "OSMODE"))
  (setq cm (getvar "CMDECHO"))
  (setq cl (getvar "CLAYER"))
  (setvar "CMDECHO" 0)

  ;; -------------------------------------------------------------------------
  ;; 도면 레이어 목록 가져오기 및 기본 레이어 자동 생성 (STEEL, 8, FIN-OUT)
  ;; -------------------------------------------------------------------------
  (defun get_layer_list (/ tbl lst name)
    (setq tbl (tblnext "LAYER" T))
    (while tbl
      (setq name (cdr (assoc 2 tbl)))
      (if (not (wcmatch name "*|*"))
        (setq lst (cons name lst))
      )
      (setq tbl (tblnext "LAYER"))
    )
    (vl-sort lst '<)
  )

  (setq lay_list (get_layer_list))
  (if (not lay_list) (setq lay_list (list "0")))

  (foreach l '("STEEL" "8" "FIN-OUT")
    (if (not (vl-position l lay_list))
      (progn
        (command "_.LAYER" "_M" l "")
        (setq lay_list (cons l lay_list))
      )
    )
  )
  (setq lay_list (vl-sort lay_list '<))

  (setq lay_str (car lay_list))
  (foreach x (cdr lay_list)
    (setq lay_str (strcat lay_str "\\n" x))
  )

  ;; -------------------------------------------------------------------------
  ;; 요약 텍스트 생성 함수 (미리보기 및 즐겨찾기 버튼 라벨 공용)
  ;; -------------------------------------------------------------------------
  (defun make_summary_str (b_t f_t c_t c_opt / b_s t_s c_s)
    (setq b_s (nth b_t '("M-BAR" "CLIP-BAR" "T-BAR")))
    (cond
      ((= f_t 0) (setq t_s "12.0mm(T12텍스)"))
      ((= f_t 1) (setq t_s "0.5mm(금속천장재)"))
      ((= f_t 2) (setq t_s "19.0mm(석고9.5T 2P)"))
      ((= f_t 3) (setq t_s "22.0mm(석고12.5+9.5T)"))
      ((= f_t 4) (setq t_s "25.0mm(석고12.5T 2P)"))
      ((= f_t 5) (setq t_s "25.0mm(T25흡음보드)"))
      ((= f_t 6) (setq t_s (strcat (rtos c_t 2 1) "mm(사용자)")))
    )
    (setq c_s (nth c_opt '("커튼:없음(양쪽몰딩)" "커튼:좌측(우측몰딩)" "커튼:우측(좌측몰딩)" "커튼:양쪽모두")))
    (strcat b_s " | " t_s " | " c_s)
  )

  ;; -------------------------------------------------------------------------
  ;; 환경 변수 및 즐겨찾기 로드/세이브
  ;; -------------------------------------------------------------------------
  (defun load_cfg (prefix /)
    (if (not (vl-bb-ref (strcat prefix "BAR_TYPE")))
      (progn
        (vl-bb-set (strcat prefix "BAR_TYPE") "0")
        (vl-bb-set (strcat prefix "FIN_TYPE") "0")
        (vl-bb-set (strcat prefix "CUSTOM_T") "12.0")
        (vl-bb-set (strcat prefix "CB_OPT") "1")
        (vl-bb-set (strcat prefix "CB_W") "120")
        (vl-bb-set (strcat prefix "CB_D") "120")
        (vl-bb-set (strcat prefix "H_LEN") "350")
        (vl-bb-set (strcat prefix "H_PITCH") "900")
      )
    )
    (setq bar_type (atoi (vl-bb-ref (strcat prefix "BAR_TYPE")))
          fin_type (atoi (vl-bb-ref (strcat prefix "FIN_TYPE")))
          custom_t (atof (vl-bb-ref (strcat prefix "CUSTOM_T")))
          cb_opt   (atoi (vl-bb-ref (strcat prefix "CB_OPT")))
          cb_w     (atof (vl-bb-ref (strcat prefix "CB_W")))
          cb_d     (atof (vl-bb-ref (strcat prefix "CB_D")))
          h_len    (atof (vl-bb-ref (strcat prefix "H_LEN")))
          h_pitch  (atof (vl-bb-ref (strcat prefix "H_PITCH")))
    )
    (if (vl-bb-ref (strcat prefix "LAY_CB_NAME"))
      (progn
        (setq idx_cb  (vl-position (vl-bb-ref (strcat prefix "LAY_CB_NAME")) lay_list)
              idx_hb  (vl-position (vl-bb-ref (strcat prefix "LAY_HB_NAME")) lay_list)
              idx_fin (vl-position (vl-bb-ref (strcat prefix "LAY_FIN_NAME")) lay_list))
        (if (not idx_cb)  (setq idx_cb 0))
        (if (not idx_hb)  (setq idx_hb 0))
        (if (not idx_fin) (setq idx_fin 0))
      )
      (progn
        (setq idx_cb  (vl-position "STEEL" lay_list))
        (if (not idx_cb) (setq idx_cb 0))
        (setq idx_hb  (vl-position "8" lay_list))
        (if (not idx_hb) (setq idx_hb 0))
        (setq idx_fin (vl-position "FIN-OUT" lay_list))
        (if (not idx_fin) (setq idx_fin 0))
      )
    )
  )

  (defun save_cfg (prefix /)
    (vl-bb-set (strcat prefix "BAR_TYPE") (itoa bar_type))
    (vl-bb-set (strcat prefix "FIN_TYPE") (itoa fin_type))
    (vl-bb-set (strcat prefix "CUSTOM_T") (rtos custom_t 2 1))
    (vl-bb-set (strcat prefix "CB_OPT") (itoa cb_opt))
    (vl-bb-set (strcat prefix "CB_W") (rtos cb_w 2 1))
    (vl-bb-set (strcat prefix "CB_D") (rtos cb_d 2 1))
    (vl-bb-set (strcat prefix "H_LEN") (rtos h_len 2 1))
    (vl-bb-set (strcat prefix "H_PITCH") (rtos h_pitch 2 1))
    (vl-bb-set (strcat prefix "LAY_CB_NAME")  (nth idx_cb lay_list))
    (vl-bb-set (strcat prefix "LAY_HB_NAME")  (nth idx_hb lay_list))
    (vl-bb-set (strcat prefix "LAY_FIN_NAME") (nth idx_fin lay_list))
    (vl-bb-set (strcat prefix "LABEL") (make_summary_str bar_type fin_type custom_t cb_opt))
  )

  (defun get_fav_label (idx / lbl)
    (setq lbl (vl-bb-ref (strcat "FAV" (itoa idx) "_LABEL")))
    (if lbl
      (strcat (itoa idx) ". " lbl)
      (strcat (itoa idx) ". (비어 있음 - 우측 [현재 세팅 저장]을 눌러 등록하세요)")
    )
  )

  ;; 현재 세팅 불러오기
  (load_cfg "CUR_")

  ;; -------------------------------------------------------------------------
  ;; 대화상자 실행 함수 (즐겨찾기 저장 시 버튼 텍스트가 즉시 바뀌도록 루프 처리)
  ;; -------------------------------------------------------------------------
  (defun run_options_dialog (/ loop_dlg f_idx b_idx c_idx c_val status_msg)
    (setq loop_dlg T
          status_msg "")

    (while loop_dlg
      (setq dcl_file (vl-filename-mktemp "ceil_v8.dcl"))
      (setq fp (open dcl_file "w"))

      (write-line "ceil_dlg : dialog { label = \"경량철골 천정틀 단면 설정 (v8)\"; width = 76;" fp)
      (write-line "  : row {" fp)
      (write-line "    : text { label = \"Designed by 양성현\"; alignment = right; }" fp)
      (write-line "  }" fp)

      (write-line "  : row {" fp)
      (write-line "    : boxed_column { label = \"천장틀 형태 및 마감재\"; width = 38;" fp)
      (write-line "      : popup_list { key = \"bar_type\"; label = \"천장틀 형태 :\"; list = \"M-BAR\\nCLIP-BAR\\nT-BAR\"; width = 25; }" fp)
      (write-line "      : popup_list { key = \"fin_type\"; label = \"마감판 종류 :\"; list = \"1. T12 흡음텍스 (기본값)\\n2. T0.5 금속천장재\\n3. 석고보드 9.5T 2P\\n4. 석고보드 12.5T+9.5T\\n5. 석고보드 12.5T 2P\\n6. T25 흡음보드\\n7. 사용자지정 (수치입력)\"; width = 25; }" fp)
      (write-line "      : edit_box { key = \"custom_t\"; label = \"사용자 두께(mm) :\"; width = 25; }" fp)
      (write-line "    }" fp)

      (write-line "    : boxed_column { label = \"커튼박스 및 몰딩 옵션\"; width = 36;" fp)
      (write-line "      : popup_list { key = \"cb_opt\"; label = \"설치 위치 :\"; list = \"설치 안 함 (양쪽 몰딩)\\n좌측만 설치 (우측 몰딩)\\n우측만 설치 (좌측 몰딩)\\n양쪽 모두 설치\"; width = 23; }" fp)
      (write-line "      : edit_box { key = \"cb_w\"; label = \"커튼박스 폭 (mm) :\"; width = 23; }" fp)
      (write-line "      : edit_box { key = \"cb_d\"; label = \"커튼박스 높이 (mm) :\"; width = 23; }" fp)
      (write-line "    }" fp)
      (write-line "  }" fp)

      (write-line "  : row {" fp)
      (write-line "    : boxed_column { label = \"규격 설정 (mm)\"; width = 38;" fp)
      (write-line "      : edit_box { key = \"h_len\"; label = \"행거볼트 높이(2점용) :\"; width = 23; }" fp)
      (write-line "      : edit_box { key = \"h_pitch\"; label = \"캐링찬넬 간격 :\"; width = 23; }" fp)
      (write-line "    }" fp)
      (write-line "    : boxed_column { label = \"항목별 레이어(Layer) 선택\"; width = 36;" fp)
      (write-line (strcat "      : popup_list { key = \"lay_cb\"; label = \"커튼박스/몰딩 :\"; list = \"" lay_str "\"; width = 22; }") fp)
      (write-line (strcat "      : popup_list { key = \"lay_hb\"; label = \"천장틀/클립 :\"; list = \"" lay_str "\"; width = 22; }") fp)
      (write-line (strcat "      : popup_list { key = \"lay_fin\"; label = \"마감재 :\"; list = \"" lay_str "\"; width = 22; }") fp)
      (write-line "    }" fp)
      (write-line "  }" fp)

      ;; 중요: AutoCAD DCL은 버튼의 label을 DCL 정의 시점에 직접 기입해야 버튼 위에 글씨가 즉시 표시됨!
      (write-line "  : boxed_column { label = \"즐겨찾기 슬롯 (좌측 버튼을 누르면 해당 세팅으로 즉시 작도 / 우측 버튼은 현재 세팅 저장)\"; width = 74;" fp)
      (write-line (strcat "    : row { : button { key = \"load1\"; label = \"" (get_fav_label 1) "\"; width = 54; } : button { key = \"save1\"; label = \"현재 세팅 저장\"; width = 16; } }") fp)
      (write-line (strcat "    : row { : button { key = \"load2\"; label = \"" (get_fav_label 2) "\"; width = 54; } : button { key = \"save2\"; label = \"현재 세팅 저장\"; width = 16; } }") fp)
      (write-line (strcat "    : row { : button { key = \"load3\"; label = \"" (get_fav_label 3) "\"; width = 54; } : button { key = \"save3\"; label = \"현재 세팅 저장\"; width = 16; } }") fp)
      (write-line (strcat "    : row { : button { key = \"load4\"; label = \"" (get_fav_label 4) "\"; width = 54; } : button { key = \"save4\"; label = \"현재 세팅 저장\"; width = 16; } }") fp)
      (write-line (strcat "    : row { : button { key = \"load5\"; label = \"" (get_fav_label 5) "\"; width = 54; } : button { key = \"save5\"; label = \"현재 세팅 저장\"; width = 16; } }") fp)
      (write-line "  }" fp)

      (write-line "  : boxed_column { label = \"실시간 미리보기\"; width = 74;" fp)
      (write-line "    : text { key = \"preview_txt\"; label = \"\"; }" fp)
      (write-line "  }" fp)

      (write-line "  ok_cancel;" fp)
      (write-line "}" fp)
      (close fp)

      (setq dcl_id (load_dialog dcl_file))
      (if (not (new_dialog "ceil_dlg" dcl_id))
        (progn (vl-file-delete dcl_file) (exit))
      )

      ;; 컨트롤 초기값 세팅
      (set_tile "bar_type" (itoa bar_type))
      (set_tile "fin_type" (itoa fin_type))
      (set_tile "custom_t" (rtos custom_t 2 1))
      (set_tile "cb_opt"   (itoa cb_opt))
      (set_tile "cb_w"     (rtos cb_w 2 1))
      (set_tile "cb_d"     (rtos cb_d 2 1))
      (set_tile "h_len"    (rtos h_len 2 1))
      (set_tile "h_pitch"  (rtos h_pitch 2 1))
      (set_tile "lay_cb"   (itoa idx_cb))
      (set_tile "lay_hb"   (itoa idx_hb))
      (set_tile "lay_fin"  (itoa idx_fin))
      (if (= fin_type 6) (mode_tile "custom_t" 0) (mode_tile "custom_t" 1))

      (defun update_preview ()
        (setq f_idx (atoi (get_tile "fin_type"))
              b_idx (atoi (get_tile "bar_type"))
              c_idx (atoi (get_tile "cb_opt"))
              c_val (atof (get_tile "custom_t")))
        (set_tile "preview_txt" (strcat "현재 세팅: " (make_summary_str b_idx f_idx c_val c_idx)))
      )

      (defun fetch_dcl_data ()
        (setq bar_type (atoi (get_tile "bar_type"))
              fin_type (atoi (get_tile "fin_type"))
              custom_t (atof (get_tile "custom_t"))
              cb_opt   (atoi (get_tile "cb_opt"))
              cb_w     (atof (get_tile "cb_w"))
              cb_d     (atof (get_tile "cb_d"))
              h_len    (atof (get_tile "h_len"))
              h_pitch  (atof (get_tile "h_pitch"))
              idx_cb   (atoi (get_tile "lay_cb"))
              idx_hb   (atoi (get_tile "lay_hb"))
              idx_fin  (atoi (get_tile "lay_fin")))
      )

      (defun save_fav_slot (idx)
        (fetch_dcl_data)
        (save_cfg (strcat "FAV" (itoa idx) "_"))
        (save_cfg "CUR_")
        (setq status_msg (strcat "[즐겨찾기 " (itoa idx) "번 저장 완료] " (make_summary_str bar_type fin_type custom_t cb_opt)))
        ;; done_dialog 2를 호출하여 새 라벨로 대화상자를 즉시 새로고침!
        (done_dialog 2)
      )

      (defun load_fav_slot (idx)
        (if (vl-bb-ref (strcat "FAV" (itoa idx) "_BAR_TYPE"))
          (progn
            (load_cfg (strcat "FAV" (itoa idx) "_"))
            (save_cfg "CUR_")
            (done_dialog 1)
          )
          (progn
            (fetch_dcl_data)
            (save_cfg (strcat "FAV" (itoa idx) "_"))
            (save_cfg "CUR_")
            (done_dialog 1)
          )
        )
      )

      (if (/= status_msg "")
        (set_tile "preview_txt" status_msg)
        (update_preview)
      )

      (action_tile "fin_type"
        "(cond
           ((= $value \"0\") (set_tile \"custom_t\" \"12.0\") (mode_tile \"custom_t\" 1))
           ((= $value \"1\") (set_tile \"custom_t\" \"0.5\")  (mode_tile \"custom_t\" 1))
           ((= $value \"2\") (set_tile \"custom_t\" \"19.0\") (mode_tile \"custom_t\" 1))
           ((= $value \"3\") (set_tile \"custom_t\" \"22.0\") (mode_tile \"custom_t\" 1))
           ((= $value \"4\") (set_tile \"custom_t\" \"25.0\") (mode_tile \"custom_t\" 1))
           ((= $value \"5\") (set_tile \"custom_t\" \"25.0\") (mode_tile \"custom_t\" 1))
           ((= $value \"6\") (mode_tile \"custom_t\" 0) (mode_tile \"custom_t\" 2))
         )
         (update_preview)"
      )
      (action_tile "bar_type" "(update_preview)")
      (action_tile "cb_opt"   "(update_preview)")
      (action_tile "custom_t" "(update_preview)")

      (action_tile "save1" "(save_fav_slot 1)")
      (action_tile "save2" "(save_fav_slot 2)")
      (action_tile "save3" "(save_fav_slot 3)")
      (action_tile "save4" "(save_fav_slot 4)")
      (action_tile "save5" "(save_fav_slot 5)")

      (action_tile "load1" "(load_fav_slot 1)")
      (action_tile "load2" "(load_fav_slot 2)")
      (action_tile "load3" "(load_fav_slot 3)")
      (action_tile "load4" "(load_fav_slot 4)")
      (action_tile "load5" "(load_fav_slot 5)")

      (action_tile "accept"
        "(fetch_dcl_data)
         (save_cfg \"CUR_\")
         (done_dialog 1)"
      )
      (action_tile "cancel" "(done_dialog 0)")

      (setq dcl_status (start_dialog))
      (unload_dialog dcl_id)
      (vl-file-delete dcl_file)

      ;; dcl_status가 2(즐겨찾기 저장)이면 while 루프를 한 번 더 돌아 새 버튼 라벨을 즉시 화면에 표시!
      (if (/= dcl_status 2)
        (setq loop_dlg nil)
      )
    )
    dcl_status
  )

  ;; -------------------------------------------------------------------------
  ;; 1. 첫 번째 점 클릭 또는 스페이스바(옵션 대화상자) / [3 / 2] 입력 루프
  ;; -------------------------------------------------------------------------
  (setq mode_val 0 ; 기본값: 3점 모드 (0=3점, 1=2점)
        pt1 nil)

  (while (not pt1)
    (setvar "OSMODE" os) ; 점 클릭 시 항상 원래 OSNAP 유지
    (initget "3 2 O")
    (setq inp (getpoint (strcat "\n천장 시작점(좌측) 클릭 (" (if (= mode_val 0) "3점모드" "2점모드") ") [3점(3)/2점(2)/옵션(스페이스바)] <옵션>: ")))
    (cond
      ((= inp "3")
        (setq mode_val 0)
        (princ "\n-> [3점 입력 모드]로 전환되었습니다. 시작점을 클릭하세요.")
      )
      ((= inp "2")
        (setq mode_val 1)
        (princ "\n-> [2점 입력 모드]로 전환되었습니다. 시작점을 클릭하세요.")
      )
      ;; 스페이스바(Enter -> nil 반환) 또는 O 입력 시 옵션 대화상자 실행!
      ((or (null inp) (= inp "O"))
        (if (= (run_options_dialog) 0)
          (exit) ; 대화상자에서 Cancel 누르면 명령 종료
        )
      )
      ((listp inp)
        (setq pt1 inp)
      )
    )
  )

  ;; 두 번째 점 클릭
  (setvar "OSMODE" os)
  (setq pt2 (getpoint pt1 "\n천장 끝점(우측) 클릭: "))
  (if (not pt2) (exit))

  ;; 3점 모드일 경우 세 번째 점(행거볼트 최종 높이점) 클릭
  (if (= mode_val 0)
    (progn
      (setvar "OSMODE" os)
      (setq pt3 (getpoint pt2 "\n행거볼트 최종 높이점(슬래브) 클릭: "))
      (if pt3
        (setq h_dyn_len (- (cadr pt3) (cadr pt1)))
        (setq h_dyn_len h_len)
      )
      (if (< h_dyn_len 50.0) (setq h_dyn_len h_len))
    )
    (setq h_dyn_len h_len)
  )

  (if (<= (car pt1) (car pt2))
    (setq p1 pt1 p2 (list (car pt2) (cadr pt1) 0.0))
    (setq p1 pt2 p2 (list (car pt1) (cadr pt2) 0.0))
  )
  (setq y_base (cadr p1))

  ;; 작도 중 간섭 방지를 위해 OSNAP 임시 해제
  (setvar "OSMODE" 0)

  (setq lay_cb  (nth idx_cb lay_list)
        lay_hb  (nth idx_hb lay_list)
        lay_fin (nth idx_fin lay_list))

  ;; 마감판 두께 계산
  (cond
    ((= fin_type 0) (setq fin_t1 12.0 fin_t2 0.0  fin_total 12.0))
    ((= fin_type 1) (setq fin_t1 0.5  fin_t2 0.0  fin_total 0.5))
    ((= fin_type 2) (setq fin_t1 9.5  fin_t2 9.5  fin_total 19.0))
    ((= fin_type 3) (setq fin_t1 12.5 fin_t2 9.5  fin_total 22.0))
    ((= fin_type 4) (setq fin_t1 12.5 fin_t2 12.5 fin_total 25.0))
    ((= fin_type 5) (setq fin_t1 25.0 fin_t2 0.0  fin_total 25.0))
    ((= fin_type 6)
      (if (<= custom_t 0.0) (setq custom_t 12.0))
      (setq fin_t1 custom_t fin_t2 0.0 fin_total custom_t)
    )
  )

  ;; M-BAR / CLIP-BAR / T-BAR 모두 수평 바(찬넬) 형태는 동일하게 배치
  (setq hbar_y1 (+ y_base fin_total 20.0)
        hbar_y2 (+ hbar_y1 38.0))

  ;; =========================================================================
  ;; 서브루틴 1: 몰딩 작도 (커튼박스 미설치 시)
  ;; =========================================================================
  (defun draw_molding_L (x yb)
    (command "_.PLINE"
             (list x (+ yb 16.0) 0.0)
             (list x (+ yb 4.0) 0.0)
             (list (+ x 10.0) (+ yb 4.0) 0.0)
             (list (+ x 10.0) (- yb 8.0) 0.0)
             (list (+ x 20.0) (- yb 8.0) 0.0)
             (list (+ x 20.0) yb 0.0)
             "")
  )

  (defun draw_molding_R (x yb)
    (command "_.PLINE"
             (list x (+ yb 16.0) 0.0)
             (list x (+ yb 4.0) 0.0)
             (list (- x 10.0) (+ yb 4.0) 0.0)
             (list (- x 10.0) (- yb 8.0) 0.0)
             (list (- x 20.0) (- yb 8.0) 0.0)
             (list (- x 20.0) yb 0.0)
             "")
  )

  ;; =========================================================================
  ;; 서브루틴 2: 클립바 클립 & T바 클립 정밀 작도 (첨부 이미지 우측 형상 100% 반영)
  ;; =========================================================================
  (defun draw_special_clip (cx yb f_tot by1 by2 b_type / cl_x y_ft b_cy)
    (setq cl_x (+ cx 50.0)
          y_ft (+ yb f_tot))

    (cond
      ;; ---------------------------------------------------------------------
      ;; [우측 CLIP-BAR 형상] 상단 사선 와이어 + 하단 역삼각형 이중 X자 다리 & 중앙 심재
      ;; ---------------------------------------------------------------------
      ((= b_type 1)
        ;; 1) 상단 걸림턱 + 수평바 가로지르는 사선 와이어 (좌하단 꺾임 포함)
        (command "_.PLINE"
                 (list (- cl_x 8.0) (- by1 4.9) 0.0)
                 (list (- cl_x 10.8) by1 0.0)
                 (list (+ cl_x 0.3) (+ by2 0.9) 0.0)
                 (list (+ cl_x 1.8) (+ by2 0.9) 0.0)
                 (list (+ cl_x 2.0) by2 0.0)
                 "")
        (command "_.PLINE"
                 (list (- cl_x 8.0) (- by1 4.9) 0.0)
                 (list (- cl_x 7.1) (- by1 4.4) 0.0)
                 (list (- cl_x 9.6) by1 0.0)
                 (list (+ cl_x 1.0) by2 0.0)
                 "")
        ;; 2) 하부 역삼각형 이중 X자 집게 다리 (상부 가로바 + 좌/우 교차 사선 날개)
        ;;    외곽 라인: 좌측 하단 다리 끝 -> 우측 상단 모서리 -> 좌측 상단 모서리 -> 우측 하단 다리 끝
        (command "_.PLINE"
                 (list (- cl_x 2.3) (+ y_ft 1.3) 0.0)
                 (list (- cl_x 3.2) (+ y_ft 1.8) 0.0)
                 (list (- cl_x 1.2) (+ y_ft 5.1) 0.0)
                 (list (- cl_x 9.8) by1 0.0)
                 (list (+ cl_x 9.8) by1 0.0)
                 (list (+ cl_x 1.2) (+ y_ft 5.1) 0.0)
                 (list (+ cl_x 3.2) (+ y_ft 1.8) 0.0)
                 (list (+ cl_x 2.3) (+ y_ft 1.3) 0.0)
                 (list (+ cl_x 0.3) (+ y_ft 4.6) 0.0)
                 (list (- cl_x 8.2) (- by1 1.0) 0.0)
                 (list (+ cl_x 8.2) (- by1 1.0) 0.0)
                 (list (- cl_x 0.3) (+ y_ft 4.6) 0.0)
                 "_C")
        ;; 3) 중앙 수직 심재 (마감판 상단 y_ft에서 위로 13mm 올라오는 2줄 수직봉 + 상단 캡)
        (command "_.RECTANG"
                 (list (- cl_x 0.3) y_ft 0.0)
                 (list (+ cl_x 0.3) (+ y_ft 13.0) 0.0))
      )

      ;; ---------------------------------------------------------------------
      ;; [우측 T-BAR 형상] 마감판 내부 분할선 제거 + 하부 플랜지 상단을 마감판 밑면(yb)에 일치
      ;; ---------------------------------------------------------------------
      ((= b_type 2)
        ;; 1) 하부 T-Bar 역T자 플랜지 (플랜지 양끝 상단이 정확히 마감판 밑면 yb에 맞닿음)
        (command "_.PLINE"
                 (list (- cl_x 12.0) yb 0.0)
                 (list (- cl_x 13.0) yb 0.0)
                 (list (- cl_x 13.0) (- yb 1.9) 0.0)
                 (list (+ cl_x 13.0) (- yb 1.9) 0.0)
                 (list (+ cl_x 13.0) yb 0.0)
                 (list (+ cl_x 12.0) yb 0.0)
                 (list (+ cl_x 12.0) (- yb 1.2) 0.0)
                 (list (+ cl_x 0.6)  (- yb 1.2) 0.0)
                 (list (+ cl_x 0.6)  (+ y_ft 4.5) 0.0)
                 (list (- cl_x 0.6)  (+ y_ft 4.5) 0.0)
                 (list (- cl_x 0.6)  (- yb 1.2) 0.0)
                 (list (- cl_x 12.0) (- yb 1.2) 0.0)
                 "_C")
        ;; 2) T-Bar 상부 벌브를 감싸는 사각 클립 요크(Yoke) (이중선)
        (command "_.PLINE"
                 (list (- cl_x 0.6) (+ y_ft 3.5) 0.0)
                 (list (- cl_x 4.0) (+ y_ft 5.0) 0.0)
                 (list (- cl_x 4.0) (+ y_ft 12.5) 0.0)
                 (list (- cl_x 1.8) (+ y_ft 14.0) 0.0)
                 (list (- cl_x 1.8) (+ by2 3.0) 0.0)
                 (list (+ cl_x 2.2) (+ by2 3.0) 0.0)
                 (list (+ cl_x 2.2) (+ by2 2.2) 0.0)
                 (list (- cl_x 1.0) (+ by2 2.2) 0.0)
                 (list (- cl_x 1.0) (+ y_ft 13.2) 0.0)
                 (list (- cl_x 3.2) (+ y_ft 12.0) 0.0)
                 (list (- cl_x 3.2) (+ y_ft 5.8) 0.0)
                 (list (- cl_x 0.6) (+ y_ft 4.5) 0.0)
                 "")
        (command "_.PLINE"
                 (list (+ cl_x 0.6) (+ y_ft 3.5) 0.0)
                 (list (+ cl_x 4.0) (+ y_ft 5.0) 0.0)
                 (list (+ cl_x 4.0) (+ y_ft 12.5) 0.0)
                 (list (+ cl_x 1.8) (+ y_ft 14.0) 0.0)
                 (list (+ cl_x 1.8) (+ by2 12.0) 0.0)
                 (list (+ cl_x 1.0) (+ by2 12.0) 0.0)
                 (list (+ cl_x 1.0) (+ y_ft 13.2) 0.0)
                 (list (+ cl_x 3.2) (+ y_ft 12.0) 0.0)
                 (list (+ cl_x 3.2) (+ y_ft 5.8) 0.0)
                 (list (+ cl_x 0.6) (+ y_ft 4.5) 0.0)
                 "")
        (command "_.LINE" (list (- cl_x 3.2) (+ y_ft 12.0) 0.0) (list (+ cl_x 3.2) (+ y_ft 12.0) 0.0) "")
        ;; 3) 수평바 하부 부근 클립 고정 볼트/너트 디테일
        (setq b_cy (+ by1 5.0))
        (command "_.PLINE"
                 (list (- cl_x 1.8) (+ b_cy 2.2) 0.0)
                 (list (- cl_x 3.8) (+ b_cy 2.2) 0.0)
                 (list (- cl_x 4.5) (+ b_cy 0.5) 0.0)
                 (list (- cl_x 3.5) (+ b_cy 0.5) 0.0)
                 (list (- cl_x 3.5) (- b_cy 0.5) 0.0)
                 (list (- cl_x 4.5) (- b_cy 0.5) 0.0)
                 (list (- cl_x 3.8) (- b_cy 2.2) 0.0)
                 (list (- cl_x 1.8) (- b_cy 2.2) 0.0)
                 "")
        (command "_.RECTANG" (list (+ cl_x 1.8) (- b_cy 2.8) 0.0) (list (+ cl_x 3.2) (+ b_cy 2.8) 0.0))
        (command "_.RECTANG" (list (- cl_x 1.8) (- b_cy 1.2) 0.0) (list (+ cl_x 5.0) (+ b_cy 1.2) 0.0))
      )
    )
  )

  ;; =========================================================================
  ;; 서브루틴 3: 캐링찬넬 + 행거볼트 (하단 너트 밑면 마감)
  ;; =========================================================================
  (defun draw_carrying_hanger (cx yb f_tot by1 by2 h_top b_type /
                               ch_bot ch_mid collar_y1 collar_y2
                               nut_w nut_h col_w y_top)
    (setq col_w     10.0
          ch_bot    (- by1 2.0)
          ch_mid    (+ by2 10.0)
          collar_y1 (+ ch_bot 106.0)
          collar_y2 (+ ch_bot 108.0)
          nut_w     8.0
          nut_h     3.5
          y_top     (+ yb h_top))

    (if (< y_top (+ collar_y2 30.0))
      (setq y_top (+ collar_y2 50.0))
    )

    ;; 캐링찬넬 및 마이너행거 본체
    (command "_.RECTANG" (list (- cx col_w) ch_bot 0.0) (list (+ cx col_w) collar_y2 0.0))
    (command "_.LINE" (list (- cx col_w) ch_mid 0.0) (list (+ cx col_w) ch_mid 0.0) "")
    (command "_.LINE" (list (- cx col_w) collar_y1 0.0) (list (+ cx col_w) collar_y1 0.0) "")

    ;; 상단 너트
    (command "_.RECTANG" (list (- cx nut_w) collar_y2 0.0) (list (+ cx nut_w) (+ collar_y2 nut_h) 0.0))
    (command "_.LINE" (list (- cx 5.5) collar_y2 0.0) (list (- cx 5.5) (+ collar_y2 nut_h) 0.0) "")
    (command "_.LINE" (list (+ cx 5.5) collar_y2 0.0) (list (+ cx 5.5) (+ collar_y2 nut_h) 0.0) "")

    ;; 하단 너트
    (command "_.RECTANG" (list (- cx nut_w) (- collar_y1 nut_h) 0.0) (list (+ cx nut_w) collar_y1 0.0))
    (command "_.LINE" (list (- cx 5.5) (- collar_y1 nut_h) 0.0) (list (- cx 5.5) collar_y1 0.0) "")
    (command "_.LINE" (list (+ cx 5.5) (- collar_y1 nut_h) 0.0) (list (+ cx 5.5) collar_y1 0.0) "")

    ;; 전산볼트(행거볼트) 단선 (하단 너트 밑면에서 깔끔하게 마감)
    (command "_.LINE" (list cx (- collar_y1 nut_h) 0.0) (list cx y_top 0.0) "")

    ;; 클립바(1) 또는 T바(2) 선택 시 캐링찬넬 우측 50mm 지점에 클립 작도 (공간 부족 시 생략)
    (if (and (or (= b_type 1) (= b_type 2))
             (<= (+ cx 50.0 15.0) bar_x2))
      (draw_special_clip cx yb f_tot by1 by2 b_type)
    )
  )

  ;; =========================================================================
  ;; [Step 1] 커튼박스 또는 몰딩 작도
  ;; =========================================================================
  (setvar "CLAYER" lay_cb)
  (setq c_p1 p1 c_p2 p2)

  ;; 좌측: 커튼박스(1, 3) vs 몰딩(0, 2)
  (if (or (= cb_opt 1) (= cb_opt 3))
    (progn
      (command "_.PLINE"
               (list (car p1) (+ y_base 4.0) 0.0)
               (list (car p1) (- y_base 8.0) 0.0)
               (list (+ (car p1) 12.0) (- y_base 8.0) 0.0)
               (list (+ (car p1) 12.0) (+ (- y_base 8.0) cb_d) 0.0)
               (list (+ (car p1) 12.0 cb_w) (+ (- y_base 8.0) cb_d) 0.0)
               (list (+ (car p1) 12.0 cb_w) (- y_base 8.0) 0.0)
               (list (+ (car p1) 12.0 cb_w 16.0) (- y_base 8.0) 0.0)
               (list (+ (car p1) 12.0 cb_w 16.0) y_base 0.0)
               (list (+ (car p1) 12.0 cb_w 10.0) y_base 0.0)
               "")
      (setq c_p1 (list (+ (car p1) 12.0 cb_w 10.0) y_base 0.0))
    )
    (progn
      (draw_molding_L (car p1) y_base)
      (setq c_p1 (list (+ (car p1) 15.0) y_base 0.0))
    )
  )

  ;; 우측: 커튼박스(2, 3) vs 몰딩(0, 1)
  (if (or (= cb_opt 2) (= cb_opt 3))
    (progn
      (command "_.PLINE"
               (list (car p2) (+ y_base 4.0) 0.0)
               (list (car p2) (- y_base 8.0) 0.0)
               (list (- (car p2) 12.0) (- y_base 8.0) 0.0)
               (list (- (car p2) 12.0) (+ (- y_base 8.0) cb_d) 0.0)
               (list (- (car p2) 12.0 cb_w) (+ (- y_base 8.0) cb_d) 0.0)
               (list (- (car p2) 12.0 cb_w) (- y_base 8.0) 0.0)
               (list (- (car p2) 12.0 cb_w 16.0) (- y_base 8.0) 0.0)
               (list (- (car p2) 12.0 cb_w 16.0) y_base 0.0)
               (list (- (car p2) 12.0 cb_w 10.0) y_base 0.0)
               "")
      (setq c_p2 (list (- (car p2) 12.0 cb_w 10.0) y_base 0.0))
    )
    (progn
      (draw_molding_R (car p2) y_base)
      (setq c_p2 (list (- (car p2) 15.0) y_base 0.0))
    )
  )

  ;; =========================================================================
  ;; [Step 2] 마감판 작도
  ;; =========================================================================
  (setvar "CLAYER" lay_fin)
  (command "_.RECTANG" (list (car c_p1) y_base 0.0) (list (car c_p2) (+ y_base fin_total) 0.0))

  (if (> fin_t2 0.0)
    (command "_.LINE" (list (car c_p1) (+ y_base fin_t1) 0.0) (list (car c_p2) (+ y_base fin_t1) 0.0) "")
  )

  ;; =========================================================================
  ;; [Step 3] 천장틀 수평 바(3종 동일 형태) 및 캐링찬넬 + 클립 작도
  ;; =========================================================================
  (setvar "CLAYER" lay_hb)
  (setq bar_x1 (+ (car c_p1) 16.0)
        bar_x2 (- (car c_p2) 16.0)
        ceil_w (- bar_x2 bar_x1))

  (if (> ceil_w 40.0)
    (progn
      ;; M-BAR / CLIP-BAR / T-BAR 모두 수평 바 본체 형태는 동일
      (command "_.RECTANG" (list bar_x1 hbar_y1 0.0) (list bar_x2 hbar_y2 0.0))

      (setq num_h (fix (/ ceil_w h_pitch)))
      (if (< num_h 1) (setq num_h 1))
      (setq span_h (* (1- num_h) h_pitch))
      (setq start_h (+ bar_x1 (/ (- ceil_w span_h) 2.0)))

      (setq cur_x start_h)
      (repeat num_h
        (draw_carrying_hanger cur_x y_base fin_total hbar_y1 hbar_y2 h_dyn_len bar_type)
        (setq cur_x (+ cur_x h_pitch))
      )
    )
  )

  (setvar "OSMODE" os)
  (setvar "CMDECHO" cm)
  (setvar "CLAYER" cl)
  (princ "\n[완료] 경량철골천정틀 v8 단면이 정상적으로 작도되었습니다.")
  (princ)
)
