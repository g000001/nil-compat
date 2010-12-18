;;; Date and time routines -*- Mode:LISP; Package:TIME; Readtable:T; Base:10 -*-
;;;	** (c) Copyright 1980 Massachusetts Institute of Technology **

;;; Note: days and months are kept one-based throughout, as much as possible.
;;; Days of the week are zero-based on Monday.

;;; [Maybe this should have a global variable which causes it to use AM/PM in place
;;;  of 24-hour time, in all relevant functions?]
;; this should probably have variable which is the initial-year,
;; in case we want more precision.

(defpackage :time
  (:use :cl))

(in-package :time)

(DEFINE-SITE-VARIABLE *TIMEZONE* :TIMEZONE "The timezone.")

;; these should probably be site variables too
(DEFVAR *DAYLIGHT-SAVINGS-TIME-P-FUNCTION* 'DAYLIGHT-SAVINGS-TIME-IN-NORTH-AMERICA-P
  "A function, which when applied to arguments of seconds minutes hours day month year,
will return T if daylight savings time is in effect in the local timezonew at that time.")

(DEFVAR *DEFAULT-DATE-PRINT-MODE* :MM//DD//YY	;perhaps site variable?
  "Defines the default way to print the date. Possible values include:
:DD//MM//YY :MM//DD//YY :DD-MM-YY :DD-MMM-YY :|DD MMM YY| :DDMMMYY :YYMMDD :YYMMMDD
 and similar keywords with YYYY instead of YY.")

(DEFUN MICROSECOND-TIME (&AUX (INHIBIT-SCHEDULING-FLAG T))
  "Return the current value of the microsecond clock (a bignum).
Only differences in clock values are meaningful.
There are 32. bits of data, so the value wraps around every few hours."
  (SELECT-PROCESSOR
    (:CADR
     (LET ((LOW (%UNIBUS-READ #o764120))  ;Hardware synchronizes if you read this one first
	   (HIGH (%UNIBUS-READ #o764122)))
       (DPB HIGH #o2020 LOW)))
    (:LAMBDA
     (COMPILER:%MICROSECOND-TIME))))

(DEFUN FIXNUM-MICROSECOND-TIME (&AUX (INHIBIT-SCHEDULING-FLAG T))
  "Return the current value of the microsecond clock as two fixnums."
  (DECLARE (VALUES LOW-23-BITS TOP-9-BITS))
  (SELECT-PROCESSOR
    (:CADR
     (LET ((LOW (%UNIBUS-READ #o764120))
	   (HIGH (%UNIBUS-READ #o764122)))
       (VALUES (DPB HIGH #o2007 LOW) (LDB #o0711 HIGH))))
    (:LAMBDA
     (LET ((TIME (COMPILER:%MICROSECOND-TIME)))
       (VALUES (LDB #o0027 TIME) (LDB #o2711 TIME))))))

(DEFCONST INTERNAL-TIME-UNITS-PER-SECOND 60.
  "60 60ths of a second in a second.")

(DEFVAR HIGH-TIME-BITS 0
  "Number of times (TIME) has wrapped around since booting.")

(DEFVAR WAS-NEGATIVE NIL
  "T if (TIME) was TIME-LESSP than LAST-BOOT-TIME when last checked.
Each this changes from T to NIL, (TIME) has wrapped around once.")

(DEFVAR LAST-BOOT-TIME 0 "Value of (TIME) when machine was booted.")

(DEFF GET-INTERNAL-REAL-TIME 'GET-INTERNAL-RUN-TIME)
(DEFUN GET-INTERNAL-RUN-TIME ()
  "Returns time in 60'ths since last boot. May be a bignum."
  (LET ((TIME-DIFF (%POINTER-DIFFERENCE (TIME) LAST-BOOT-TIME)))
    (WHEN (AND (PROG1 WAS-NEGATIVE
		      (SETQ WAS-NEGATIVE (LDB-TEST (BYTE 1 22.) TIME-DIFF)))
	       (NOT WAS-NEGATIVE))
      (INCF HIGH-TIME-BITS))
    (DPB HIGH-TIME-BITS (BYTE 23. 23.) (LDB (BYTE 23. 0) TIME-DIFF))))


;;;; Conversion routines, universal time is seconds since 1-jan-00 00:00-GMT

(DEFVAR *CUMULATIVE-MONTH-DAYS-TABLE*
	(MAKE-ARRAY 13. :TYPE 'ART-16B
		        :INITIAL-CONTENTS '#10r(0 0   31  59  90  120 151
						  181 212 243 273 304 334))
  "One-based array of cumulative days per month.")

;;; Takes Univeral Time (seconds since 1/1/1900) as a 32-bit number
;;; Algorithm from KLH's TIMRTS.
(DEFUN DECODE-UNIVERSAL-TIME (UNIVERSAL-TIME &OPTIONAL TIMEZONE
					     &AUX SECS MINUTES HOURS DAY MONTH YEAR
						  DAY-OF-THE-WEEK DAYLIGHT-SAVINGS-TIME-P)
  "Given a UNIVERSAL-TIME, decode it into year, month number, day of month, etc.
TIMEZONE is hours before GMT (5, for EST).
DAY and MONTH are origin-1.  DAY-OF-THE-WEEK = 0 for Monday."
  (DECLARE (VALUES SECS MINUTES HOURS DAY MONTH YEAR
		   DAY-OF-THE-WEEK DAYLIGHT-SAVINGS-TIME-P TIMEZONE))
  (IF TIMEZONE					;explicit timezone means no-dst
      (MULTIPLE-VALUE (SECS MINUTES HOURS DAY MONTH YEAR DAY-OF-THE-WEEK)
	 (DECODE-UNIVERSAL-TIME-WITHOUT-DST UNIVERSAL-TIME TIMEZONE))
    ;;Otherwise, decode the time and THEN daylight-adjust it.
    (MULTIPLE-VALUE (SECS MINUTES HOURS DAY MONTH YEAR DAY-OF-THE-WEEK)
      (DECODE-UNIVERSAL-TIME-WITHOUT-DST UNIVERSAL-TIME *TIMEZONE*))
    (AND (SETQ DAYLIGHT-SAVINGS-TIME-P
	       (FUNCALL *DAYLIGHT-SAVINGS-TIME-P-FUNCTION*
			SECS MINUTES HOURS DAY MONTH YEAR))
	 ;; See if it's daylight savings time, time-zone number gets smaller if so.
	 (MULTIPLE-VALUE (SECS MINUTES HOURS DAY MONTH YEAR DAY-OF-THE-WEEK)
	   (DECODE-UNIVERSAL-TIME-WITHOUT-DST UNIVERSAL-TIME (1- *TIMEZONE*)))))
  (VALUES SECS MINUTES HOURS DAY MONTH YEAR DAY-OF-THE-WEEK DAYLIGHT-SAVINGS-TIME-P
	  (OR TIMEZONE *TIMEZONE*)))

(DEFUN DECODE-UNIVERSAL-TIME-WITHOUT-DST (UNIVERSAL-TIME &OPTIONAL (TIMEZONE *TIMEZONE*)
					  &AUX X SECS MINUTES HOURS DAY MONTH YEAR)
  "Like DECODE-UNIVERSAL-TIME, but always uses standard time.
Even if the time is one at which daylight savings time would be in effect,
the hour and date are computed as for standard time."
  (DECLARE (VALUES SECS MINUTES HOURS DAY MONTH YEAR DAY-OF-THE-WEEK TIMEZONE))
  (SETQ UNIVERSAL-TIME (- UNIVERSAL-TIME (* TIMEZONE 3600.)))
  (SETQ SECS (\ UNIVERSAL-TIME (* 24. 60. 60.))
	X (TRUNCATE UNIVERSAL-TIME (* 24. 60. 60.)))	;Days since genesis.
  (MULTIPLE-VALUE-BIND (A B) (FLOOR X 365.)
    (UNLESS (ZEROP A)
      (DECF B (LSH (1- A) -2))
      (WHEN (< B 0)
	(SETQ A (+ A -1 (TRUNCATE B 365.)))	;We must allow for times so far in the future
	(SETQ B (\ B 365.))			;as to produce >> 365. Feb 29's.
	(SETQ B (+ B 365.))			;(Of course, this doesn't allow for
						;the year 2100 not being a leap-year.)
	(AND (NOT (BIT-TEST A 3))
	     (INCF B))))
    (DO ((C 12. (1- C)))
	(( B (AREF *CUMULATIVE-MONTH-DAYS-TABLE* C))
	 (WHEN (AND (NOT (BIT-TEST A 3))
		    (> C 2))
	   (DECF B)
	   (IF (< B (AREF *CUMULATIVE-MONTH-DAYS-TABLE* C)) (DECF C))
	   (IF (= C 2) (INCF B)))
	 (DECF B (AREF *CUMULATIVE-MONTH-DAYS-TABLE* C))
	 (SETQ YEAR (+ 1900. A))
	 (SETQ MONTH C)
	 (SETQ DAY (1+ B)))))
  (SETQ HOURS (FLOOR SECS 3600.)
	MINUTES (FLOOR (\ SECS 3600.) 60.)
	SECS (\ SECS 60.))
  (VALUES SECS MINUTES HOURS DAY MONTH YEAR (\ X 7) TIMEZONE))

(DEFUN DAYLIGHT-SAVINGS-TIME-P (&REST ARGS)
  "T if daylight savings time would be in effect at specified time in the local timezone."
  (DECLARE (ARGLIST HOURS DAY MONTH YEAR))
  (APPLY *DAYLIGHT-SAVINGS-TIME-P-FUNCTION* 0 0 ARGS))

(DEFUN YYYY-YY (YEAR CURRENT-YEAR)
  (IF (= (TRUNCATE (+ YEAR 50.) 100.) (TRUNCATE (+ CURRENT-YEAR 50.) 100.))
      (MOD YEAR 100.)
    YEAR))

 (DEFUN ENCODE-UNIVERSAL-TIME (SECONDS MINUTES HOURS DAY MONTH YEAR
			      &OPTIONAL TIMEZONE &AUX TEM)
  "Given a time, return a universal-time encoding of it.
A universal-time is the number of seconds since 1-Jan-1900 00:00-GMT (a bignum)."
  (IF (< YEAR 100.)
      (LET ((CURRENT-YEAR (NTH-VALUE 5 (GET-DECODED-TIME))))
	;; In case called during startup or during DISK-SAVE.
	(UNLESS CURRENT-YEAR
	  (SETQ CURRENT-YEAR 2000.))
	(SETQ YEAR
	      (+ CURRENT-YEAR
		 (- (MOD (+ 50. (- YEAR (\ CURRENT-YEAR 100.))) 100.) 50.)))))
  (SETQ YEAR (- YEAR 1900.))
  (OR TIMEZONE
      (SETQ TIMEZONE (IF (DAYLIGHT-SAVINGS-TIME-P HOURS DAY MONTH YEAR)
			 (1- *TIMEZONE*) *TIMEZONE*)))
  (SETQ TEM (+ (1- DAY) (AREF *CUMULATIVE-MONTH-DAYS-TABLE* MONTH)
	       (FLOOR (1- YEAR) 4) (* YEAR 365.)))	;Number of days since 1-Jan-1900.
  (AND (> MONTH 2) (LEAP-YEAR-P YEAR)
       (SETQ TEM (1+ TEM)))				;After 29-Feb in a leap year.
  (+ SECONDS (* 60. MINUTES) (* 3600. HOURS) (* TEM (* 60. 60. 24.)) (* TIMEZONE 3600.)))


;;;; domain-dependent knowledge
(DEFUN DAYLIGHT-SAVINGS-TIME-IN-NORTH-AMERICA-P (SECONDS MINUTES HOURS DAY MONTH YEAR)
  "T if daylight savings time would be in effect at specified time in North America."
  (DECLARE (IGNORE SECONDS MINUTES))
  (COND ((OR (< MONTH 4)		;Standard time if before 2 am last Sunday in April
	     (AND (= MONTH 4)
		  (LET ((LSA (LAST-SUNDAY-IN-APRIL YEAR)))
		    (OR (< DAY LSA)
			(AND (= DAY LSA) (< HOURS 2))))))
	 NIL)
	((OR (> MONTH 10.)		;Standard time if after 1 am last Sunday in October
	     (AND (= MONTH 10.)
		  (LET ((LSO (LAST-SUNDAY-IN-OCTOBER YEAR)))
		    (OR (> DAY LSO)
			(AND (= DAY LSO) ( HOURS 1))))))
	 NIL)
	(T T)))

(DEFUN LAST-SUNDAY-IN-OCTOBER (YEAR)
  (LET ((LSA (LAST-SUNDAY-IN-APRIL YEAR)))
    ;; Days between April and October = 31+30+31+31+30 = 153  6 mod 7
    ;; Therefore the last Sunday in October is one less than the last Sunday in April
    ;; unless that gives 24. or 23. in which case it is six greater.
    (IF ( LSA 25.) (+ LSA 6) (1- LSA))))

(DEFUN LAST-SUNDAY-IN-APRIL (YEAR)
  (IF (> YEAR 100.)
      (SETQ YEAR (- YEAR 1900.)))
  ;; This copied from GDWOBY routine in ITS
  (LET ((DOW-BEG-YEAR
	  (LET ((B (\ (+ YEAR 1899.) 400.)))
	    (\ (- (+ (1+ B) (SETQ B (FLOOR B 4))) (FLOOR B 25.)) 7)))
	(FEB29 (IF (LEAP-YEAR-P YEAR) 1 0)))
    (LET ((DOW-APRIL-30 (\ (+ DOW-BEG-YEAR 119. FEB29) 7)))
      (- 30. DOW-APRIL-30))))

;;;; Maintenance functions

(DEFVAR *LAST-TIME-UPDATE-TIME* NIL
  "A number representing the universal time of the last update time.")
(DEFVAR PREVIOUS-TOP-9-TIME-BITS NIL)
(DEFVAR *LAST-TIME-SECONDS* NIL "A number representing the seconds of the last update time.")
(DEFVAR *LAST-TIME-MINUTES* NIL "A number representing the minutes of the last update time.")
(DEFVAR *LAST-TIME-HOURS* NIL "A number representing the hours of the last update time.")
(DEFVAR *LAST-TIME-DAY* NIL "A number representing the day of the last update time.")
(DEFVAR *LAST-TIME-MONTH* NIL "A number representing the month of the last update time.")
(DEFVAR *LAST-TIME-YEAR* NIL "A number representing the year of the last update time.")
(DEFVAR *LAST-TIME-DAY-OF-THE-WEEK* NIL
  "A number representing the day of the week of the last update time.")
(DEFVAR *LAST-TIME-DAYLIGHT-SAVINGS-P* NIL "Whether it was DST the last update time.")
(DEFVAR *NETWORK-TIME-FUNCTION* NIL)
(DEFVAR *UT-AT-BOOT-TIME* NIL "Used for UPTIME protocol, do not random SETQ.")

(DEFUN INITIALIZE-TIMEBASE (&OPTIONAL UT)
  "Set the clock.
Possible sources of the time include the network, the Lambda SDU clock,
and, failing that, the luser who happens to be around."
  (AND (NULL UT) (NOT (SI:GET-SITE-OPTION :STANDALONE)) *NETWORK-TIME-FUNCTION*
       (SETQ UT (FUNCALL *NETWORK-TIME-FUNCTION*)))
  (TAGBODY
      (AND (NUMBERP UT) (GO DO-IT))
      ; don't deal with SDU clock until cold load is done ...
      (if-in-lambda
	(SETQ UT (RTC-GET-UNIVERSAL-TIME))
	(IF UT (GO DO-IT)))		; change to GIVE-IT-A-SHOT if the user should be asked
   STRING
      (FORMAT *QUERY-IO* "~&Please type the date and time: ")
      (SETQ UT (READLINE *QUERY-IO*))
      (WHEN (STRING-EQUAL UT "")
	(IF (Y-OR-N-P "Do you want to specify the time or not? ")
	    (GO STRING)
	  (SETQ *LAST-TIME-UPDATE-TIME* NIL)
	  (RETURN-FROM INITIALIZE-TIMEBASE NIL)))
      (CONDITION-CASE (ERROR)
	  (SETQ UT (PARSE-UNIVERSAL-TIME UT 0 NIL T 0))
	(ERROR (SEND ERROR :REPORT *QUERY-IO*)
	       (GO STRING)))
   GIVE-IT-A-SHOT
      (COND ((NOT (Y-OR-N-P (FORMAT NIL "Time is ~A, OK? " (PRINT-UNIVERSAL-DATE UT NIL))))
	     (GO STRING)))
   DO-IT
      (WITHOUT-INTERRUPTS
	(IF (NOT (NULL *UT-AT-BOOT-TIME*))
	    ;;if we are randomly changing the time while up, mung uptime
	    (SETQ *UT-AT-BOOT-TIME*
		  (+ *UT-AT-BOOT-TIME* (- UT (GET-UNIVERSAL-TIME))))
	  ;;no real surprise: changing at boot time
	  (SETQ *UT-AT-BOOT-TIME* UT))
	(SETF (VALUES *LAST-TIME-UPDATE-TIME* PREVIOUS-TOP-9-TIME-BITS)
	      (FIXNUM-MICROSECOND-TIME))
	(MULTIPLE-VALUE (*LAST-TIME-SECONDS* *LAST-TIME-MINUTES* *LAST-TIME-HOURS*
			 *LAST-TIME-DAY* *LAST-TIME-MONTH* *LAST-TIME-YEAR*
			 *LAST-TIME-DAY-OF-THE-WEEK* *LAST-TIME-DAYLIGHT-SAVINGS-P*)
	  (DECODE-UNIVERSAL-TIME UT))
	(IF-IN-LAMBDA (RTC-SET-UNIVERSAL-TIME UT))
	(RETURN-FROM INITIALIZE-TIMEBASE T))))

(DEFUN SET-LOCAL-TIME (&OPTIONAL NEW-TIME)
  (AND (STRINGP NEW-TIME)
       (SETQ NEW-TIME (TIME:PARSE-UNIVERSAL-TIME NEW-TIME)))
  (LET ((*NETWORK-TIME-FUNCTION* NIL))
    (INITIALIZE-TIMEBASE NEW-TIME)))

;; This is so freshly booted machines don't give out an incorrect time or uptime until
;; they've found out for themselves what the time *really* is.
(ADD-INITIALIZATION "Forget time" '(SETQ TIME:*LAST-TIME-UPDATE-TIME* NIL) '(BEFORE-COLD))
(ADD-INITIALIZATION "Forget uptime" '(SETQ TIME:*UT-AT-BOOT-TIME* NIL) '(BEFORE-COLD))

;;; This must not process-wait, since it can be called inside the scheduler via the who-line
(DEFUN UPDATE-TIMEBASE (&AUX TIME TICK TOP-9-TIME-BITS INCREMENTAL-TOP-10-TIME-BITS
			(OLD-HOUR *LAST-TIME-HOURS*))
  "Update our information on the current time."
  (WHEN (NOT (NULL *LAST-TIME-UPDATE-TIME*))
    (WITHOUT-INTERRUPTS
      ;; Put the following code back if the TIME function ever makes any attempt
      ;; to be even close to 60 cycles.  Also change INITIALIZE-TIMEBASE.
      ;(SETQ TIME (TIME)
      ;	 TICK (TRUNC (TIME-DIFFERENCE TIME *LAST-TIME-UPDATE-TIME*) 60.)
      ;	 *LAST-TIME-UPDATE-TIME*
      ;	    (LDB #o0027 (%24-BIT-PLUS (* 60. TICK) *LAST-TIME-UPDATE-TIME*)))
      (SETF (VALUES TIME TOP-9-TIME-BITS)
	    (FIXNUM-MICROSECOND-TIME))
      ;; Don't lose when installing this code,
      ;; if PREVIOUS-TOP-9-TIME-BITS has not been being updated.
      (OR PREVIOUS-TOP-9-TIME-BITS
	  (SETQ PREVIOUS-TOP-9-TIME-BITS TOP-9-TIME-BITS))
      ;; See if we have "missed any ticks" in the low 23. bits;
      ;; Normally we are supposed to be called frequently enough
      ;; that bit 23. never increments twice between calls to this function
      ;; but a long WITHOUT-INTERRUPTS can make that happen.
      (SETQ INCREMENTAL-TOP-10-TIME-BITS
	    (LSH (- TOP-9-TIME-BITS PREVIOUS-TOP-9-TIME-BITS) 1)
	    PREVIOUS-TOP-9-TIME-BITS TOP-9-TIME-BITS)
      ;; In the ordinary course of events, we DO notice bit 23 increment
      ;; because we see the low 23 bits wrap around.
      ;; So don't count those noticed increments in the "extras".
      (IF (< TIME *LAST-TIME-UPDATE-TIME*)
	  (DECF INCREMENTAL-TOP-10-TIME-BITS 2))
      ;; INCREMENTAL-TOP-10-TIME-BITS is now set to twice the number of times
      ;; that bit 23 has incremented since we last ran, that we didn't notice.
      ;; Now feed that many increments into bit 22, one by one.
      ;; When finished with them (if there are any),
      ;; handle the change in the low 23 bits themselves.
      (DO (EXIT-THIS-TIME) (())
	(IF ( INCREMENTAL-TOP-10-TIME-BITS 0)
	    (SETQ TICK (FLOOR (TIME-DIFFERENCE TIME *LAST-TIME-UPDATE-TIME*) 1000000.)
		  EXIT-THIS-TIME T)
	  (SETQ TICK (FLOOR (DPB 1 #o2601 0) 1000000.)))
	(SETQ *LAST-TIME-UPDATE-TIME*
	      (LDB #o0027 (%MAKE-POINTER-OFFSET
			    DTP-FIX
			    (* 1000000. TICK) *LAST-TIME-UPDATE-TIME*)))
	(OR (ZEROP TICK)
	    (< (SETQ *LAST-TIME-SECONDS* (+ *LAST-TIME-SECONDS* TICK)) 60.)
	    (< (PROG1 (SETQ *LAST-TIME-MINUTES* (+ *LAST-TIME-MINUTES*
						   (FLOOR *LAST-TIME-SECONDS* 60.)))
		      (SETQ *LAST-TIME-SECONDS* (\ *LAST-TIME-SECONDS* 60.)))
	       60.)
	    (< (PROG1 (SETQ *LAST-TIME-HOURS* (+ *LAST-TIME-HOURS*
						 (FLOOR *LAST-TIME-MINUTES* 60.)))
		      (SETQ *LAST-TIME-MINUTES* (\ *LAST-TIME-MINUTES* 60.)))
	       24.)
	    ( (PROG1 (SETQ *LAST-TIME-DAY* (1+ *LAST-TIME-DAY*))
		      (SETQ *LAST-TIME-DAY-OF-THE-WEEK*
			    (\ (1+ *LAST-TIME-DAY-OF-THE-WEEK*) 7))
		      (SETQ *LAST-TIME-HOURS* 0))
	       (MONTH-LENGTH *LAST-TIME-MONTH* *LAST-TIME-YEAR*))
	    ( (SETQ *LAST-TIME-DAY* 1
		     *LAST-TIME-MONTH* (1+ *LAST-TIME-MONTH*))
	       12.)
	    (SETQ *LAST-TIME-MONTH* 1
		  *LAST-TIME-YEAR* (1+ *LAST-TIME-YEAR*)))
	(IF EXIT-THIS-TIME
	    (RETURN NIL)
	  (DECF INCREMENTAL-TOP-10-TIME-BITS)))
      (WHEN ( OLD-HOUR *LAST-TIME-HOURS*)
	;; If hour has incremented, turn decoded time into a UT
	;; using the timezone we were using up to now,
	;; use that to decide if we have turned DST on or off,
	;; and then re-decode the time.
	(MULTIPLE-VALUE (*LAST-TIME-SECONDS* *LAST-TIME-MINUTES* *LAST-TIME-HOURS*
			 *LAST-TIME-DAY* *LAST-TIME-MONTH* *LAST-TIME-YEAR*
			 *LAST-TIME-DAY-OF-THE-WEEK* *LAST-TIME-DAYLIGHT-SAVINGS-P*)
	  (DECODE-UNIVERSAL-TIME
	    (ENCODE-UNIVERSAL-TIME
	      *LAST-TIME-SECONDS* *LAST-TIME-MINUTES* *LAST-TIME-HOURS*
	      *LAST-TIME-DAY* *LAST-TIME-MONTH* *LAST-TIME-YEAR*
	      (IF *LAST-TIME-DAYLIGHT-SAVINGS-P*
		  (1- *TIMEZONE*) *TIMEZONE*))))
	;; Update things for GET-INTERNAL-RUN-TIME at least once an hour.
	(GET-INTERNAL-RUN-TIME))
      T)))

(DEFVAR *MONTH-LENGTHS* '#10r(0 31 28 31 30 31 30 31 31 30 31 30 31)
  "One-based list of lengths of months.")

(DEFUN MONTH-LENGTH (MONTH YEAR)
  "Return the number of days in month MONTH in year YEAR.
Knows about leap years.  January is month 1."
  (IF (= MONTH 2)
      (IF (LEAP-YEAR-P YEAR) 29. 28.)
    (NTH MONTH *MONTH-LENGTHS*)))

(DEFUN LEAP-YEAR-P (YEAR)			;2000 is a leap year.  2100 is not.
  "T if YEAR is a leap year."
  (IF (< YEAR 100.)
      (SETQ YEAR (+ 1900. YEAR)))
  (AND (ZEROP (\ YEAR 4))
       (OR (NOT (ZEROP (\ YEAR 100.)))
	   (ZEROP (\ YEAR 400.)))))

(DEFUN DAYLIGHT-SAVINGS-P ()
  "T if we are now in daylight savings time."
  (UPDATE-TIMEBASE)
  *LAST-TIME-DAYLIGHT-SAVINGS-P*)

(DEFUN DEFAULT-YEAR ()
  "Return the current year, minus 1900."
  (UPDATE-TIMEBASE)
  *LAST-TIME-YEAR*)

;;; These are the functions the user should call
;;; If they can't find out what time it is, they return NIL
(DEFF GET-DECODED-TIME 'GET-TIME)
(DEFUN GET-TIME ()
  "Return the current time, decoded into second, hour, day, etc.
Returns NIL if the time is not known (during startup or DISK-SAVE)."
  (DECLARE (VALUES SECONDS MINUTES HOURS DAY MONTH YEAR DAY-OF-THE-WEEK
		   DAYLIGHT-SAVINGS-P TIMEZONE))
  (AND (UPDATE-TIMEBASE)
       (VALUES *LAST-TIME-SECONDS* *LAST-TIME-MINUTES* *LAST-TIME-HOURS*
	       *LAST-TIME-DAY* *LAST-TIME-MONTH*
	       *LAST-TIME-YEAR*
	       *LAST-TIME-DAY-OF-THE-WEEK* *LAST-TIME-DAYLIGHT-SAVINGS-P*
	       *TIMEZONE*)))

(DEFUN GET-UNIVERSAL-TIME ()
  "Return the current time as a universal-time.
A universal-time is the number of seconds since 01-Jan-1900 00:00-GMT (a bignum)"
  (UPDATE-TIMEBASE)
  (ENCODE-UNIVERSAL-TIME *LAST-TIME-SECONDS* *LAST-TIME-MINUTES* *LAST-TIME-HOURS*
			 *LAST-TIME-DAY* *LAST-TIME-MONTH* *LAST-TIME-YEAR*
			 (IF *LAST-TIME-DAYLIGHT-SAVINGS-P*
			     (1- *TIMEZONE*) *TIMEZONE*)))


;;;args to format: DAY MONTH MONTH-STRING DONT-PRINT-YEAR-P YEAR2 YEAR4
;;;		   0   1     2            3                 4	  5
(DEFPROP :DD//MM//YY "~D//~2,'0D~*~:[//~2,'0D~]" DATE-FORMAT)		;27/10{/66}
(DEFPROP :DD//MM//YYYY "~D//~2,'0D~*~:[//~*~D~]" DATE-FORMAT)		;27/10{/1966}
(DEFPROP :MM//DD//YY "~*~D//~0@*~2,'0D~2*~:[//~2,'0D~]" DATE-FORMAT)	;10/27{/66}
(DEFPROP :MM//DD//YYYY "~*~D//~0@*~2,'0D~2*~:[//~*~D~]" DATE-FORMAT)	;10/27{/1966}
(DEFPROP :DD-MM-YY "~D-~2,'0D~*~:[-~2,'0D~]" DATE-FORMAT)		;27-10{-66}
(DEFPROP :DD-MM-YYYY "~D-~2,'0D~*~:[-~*~D~]" DATE-FORMAT)		;27-10{-1966}
(DEFPROP :DD-MMM-YY "~D-~*~A~:[-~2,'0D~]" DATE-FORMAT)			;27-Oct{-66}
(DEFPROP :DD-MMM-YYYY "~D-~*~A~:[-~*~D~]" DATE-FORMAT)			;27-Oct{-1966}
(DEFPROP :DD/ MMM/ YY "~D ~*~A~:[ ~2,'0D~]" DATE-FORMAT)		;27 Oct{ 66}
(DEFPROP :DD/ MMM/ YYYY "~D ~*~A~:[ ~*~D~]" DATE-FORMAT)		;27 Oct{ 1966}
(DEFPROP :DDMMMYY "~D~*~A~:[~2,'0D~]" DATE-FORMAT)			;27Oct{66}
(DEFPROP :DDMMMYYYY "~D~*~A~:[~*~D~]" DATE-FORMAT)			;27Oct{1966}
(DEFPROP :YYMMDD "~4*~2,'0D~1@*~2,'0D~0@*~2,'0D" DATE-FORMAT)		;661027
(DEFPROP :YYYYMMDD "~5*~2,'0D~1@*~2,'0D~0@*~2,'0D" DATE-FORMAT)		;19661027
(DEFPROP :YYMMMDD "~3*~:[~2,'0D~]~2@*~A~0@*~2,'0D" DATE-FORMAT)		;{66}Oct27
(DEFPROP :YYYYMMMDD "~3*~:[~*~D~]~2@*~A~0@*~2,'0D" DATE-FORMAT)		;{1966}Oct27
(DEFPROP :YY-MMM-DD "~3*~:[~2,'0D-~]~2@*~A-~0@*~2,'0D" DATE-FORMAT)	;{66-}Oct-27
(DEFPROP :YYYY-MMM-DD "~3*~:[~*~D-~]~2@*~A-~0@*~2,'0D" DATE-FORMAT)	;{1966-}Oct-27
(DEFPROP :YY-MM-DD "~3*~:[~2,'0D-~]~1@*~A-~0@*~2,'0D" DATE-FORMAT)	;{66-}10-27
(DEFPROP :YYYY-MM-DD "~3*~:[~*~D-~]~1@*~A-~0@*~2,'0D" DATE-FORMAT)	;{1966-}10-27

(DEFUN PRINT-CURRENT-TIME (&OPTIONAL (STREAM *STANDARD-OUTPUT*)
				     (DATE-PRINT-MODE *DEFAULT-DATE-PRINT-MODE*))
  "Print the current time on STREAM."
  (AND (UPDATE-TIMEBASE)
       (MULTIPLE-VALUE-BIND (SECONDS MINUTES HOURS DAY MONTH YEAR)
	   (GET-TIME)
         (PRINT-TIME SECONDS MINUTES HOURS DAY MONTH YEAR STREAM DATE-PRINT-MODE))))

(DEFUN PRINT-UNIVERSAL-TIME (UT &OPTIONAL (STREAM *STANDARD-OUTPUT*)
					  TIMEZONE
					  (DATE-PRINT-MODE *DEFAULT-DATE-PRINT-MODE*))
  "Print the universal-time UT on STREAM, interpreting for time zone TIMEZONE.
TIMEZONE is the number of hours earlier than GMT."
  ;;Let DECODE-UNIVERSAL-TIME default the timezone if wanted, as that fcn
  ;;must know to suppress DST iff TIMEZONE is supplied.
  (MULTIPLE-VALUE-BIND (SECONDS MINUTES HOURS DAY MONTH YEAR)
      (DECODE-UNIVERSAL-TIME UT TIMEZONE)
    (PRINT-TIME SECONDS MINUTES HOURS DAY MONTH YEAR STREAM DATE-PRINT-MODE)))

(DEFUN PRINT-TIME (SECONDS MINUTES HOURS DAY MONTH YEAR
		   &OPTIONAL (STREAM *STANDARD-OUTPUT*)
			     (DATE-PRINT-MODE *DEFAULT-DATE-PRINT-MODE*))
  "Print time specified on STREAM using date format DATE-PRINT-MODE.
If STREAM is NIL, construct and return a string."
  (WITH-STACK-LIST (DATE-MODE-ARGS DAY MONTH (MONTH-STRING MONTH :SHORT)
				   NIL (YYYY-YY YEAR (NTH-VALUE 5 (GET-DECODED-TIME))) YEAR)
    (FORMAT STREAM "~? ~2,'0D:~2,'0D:~2,'0D"
	    (OR (GET DATE-PRINT-MODE 'DATE-FORMAT)
		(FERROR NIL "Bad type of DATE-PRINT-MODE: ~S" DATE-PRINT-MODE))
	    DATE-MODE-ARGS
	    HOURS MINUTES SECONDS)))

(DEFUN PRINT-CURRENT-DATE (&OPTIONAL (STREAM *STANDARD-OUTPUT*))
  "Print the current date in a verbose form on STREAM.
If STREAM is NIL, construct and return a string."
  (AND (UPDATE-TIMEBASE)
       (MULTIPLE-VALUE-BIND (SECONDS MINUTES HOURS DAY MONTH YEAR DAY-OF-THE-WEEK)
	   (GET-TIME)
         (PRINT-DATE SECONDS MINUTES HOURS DAY MONTH YEAR DAY-OF-THE-WEEK STREAM))))

(DEFUN PRINT-UNIVERSAL-DATE (UT &OPTIONAL (STREAM *STANDARD-OUTPUT*) TIMEZONE)
  "Print the universal-time UT in verbose form on STREAM, decoding for TIMEZONE.
If STREAM is NIL, construct and return a string."
  (MULTIPLE-VALUE-BIND (SECONDS MINUTES HOURS DAY MONTH YEAR DAY-OF-THE-WEEK)
      (DECODE-UNIVERSAL-TIME UT TIMEZONE)
    (PRINT-DATE SECONDS MINUTES HOURS DAY MONTH YEAR DAY-OF-THE-WEEK STREAM)))

(DEFUN PRINT-DATE (SECONDS MINUTES HOURS DAY MONTH YEAR DAY-OF-THE-WEEK
		   &OPTIONAL (STREAM *STANDARD-OUTPUT*))
  "Print the date and time in verbose form on STREAM.
If STREAM is NIL, construct and return a string."
  (SETQ MONTH (MONTH-STRING MONTH)
	DAY-OF-THE-WEEK (DAY-OF-THE-WEEK-STRING DAY-OF-THE-WEEK))
  (FORMAT STREAM
	  "~A the ~:R of ~A, ~D; ~D:~2,'0D:~2,'0D ~A"
	  DAY-OF-THE-WEEK DAY MONTH YEAR (1+ (\ (+ HOURS 11.) 12.)) MINUTES SECONDS
	  (COND ((AND (ZEROP SECONDS)
		      (ZEROP MINUTES)
		      (MEMQ HOURS '(0 12.)))
		 (IF (= HOURS 0) "midnight" "noon"))
		(( HOURS 12.) "pm")
		(T "am"))))

(DEFUN PRINT-BRIEF-UNIVERSAL-TIME (UT &OPTIONAL (STREAM *STANDARD-OUTPUT*)
						(REF-UT (GET-UNIVERSAL-TIME))
						(DATE-PRINT-MODE *DEFAULT-DATE-PRINT-MODE*))
  "Prints only those aspects of the time, UT, that differ from the current time.
Also never prints seconds.  Used by notifications, for example.
If STREAM is NIL, construct and return a string."
  (MULTIPLE-VALUE-BIND (IGNORE MINUTES HOURS DAY MONTH YEAR)
      (DECODE-UNIVERSAL-TIME UT)
    (MULTIPLE-VALUE-BIND (IGNORE IGNORE IGNORE REF-DAY REF-MONTH REF-YEAR)
	(DECODE-UNIVERSAL-TIME REF-UT)
      ;; If not same day, print month and day numerically
      (IF (OR ( DAY REF-DAY) ( MONTH REF-MONTH) ( YEAR REF-YEAR))
	  (WITH-STACK-LIST (DATE-MODE-ARGS DAY MONTH (MONTH-STRING MONTH :SHORT)
					   (= YEAR REF-YEAR) (YYYY-YY YEAR REF-YEAR) YEAR)
	    (FORMAT STREAM "~? ~2,'0D:~2,'0D"
		    (OR (GET DATE-PRINT-MODE 'DATE-FORMAT)
			(FERROR NIL "Bad type-of DATE-PRINT-MODE: ~S" DATE-PRINT-MODE))
		    DATE-MODE-ARGS
		    HOURS MINUTES))
	;; Always print hours colon minutes, even if same as now
	(FORMAT STREAM "~2,'0D:~2,'0D" HOURS MINUTES)))))

(DEFUN PRINT-UPTIME (&OPTIONAL (STREAM *STANDARD-OUTPUT*))
  "Print how long this machine has been up since last cold boot."
  (FORMAT STREAM "~&This machine has been up ~\time-interval\."
	  (- (TIME:GET-UNIVERSAL-TIME) TIME:*UT-AT-BOOT-TIME*)))


#|
;;;; Essential stuff
(defun moonphase (&optional (ut (get-universal-time)))
;  (multiple-value-bind (seconds minutes hours date month year nil dstp)
;		       (decode-universal-time ut)
;    (let* ((secs (+ seconds (* minutes 60.) (* hours 3600.)
;		     (if dstp -3600. 0)
;		     (* (+ date -1
;			   (aref cumulative-month-days-table month))
;			 86400.)))
;	   (year-1 (1- year))
;	   (d (+ (- (+ (* year 365.) (ash year-1 -2)) (truncate year-1 100.))
;		 (truncate year-1 400.)
;		 1)))
;      ;If one wanted to waste time, it would seem to be possible to just
;      ; change the constants which follow, and just use the universal
;      ; time in place of all the code above and the quantity
;      ; (+ (* d 86400.) secs) below.  Possibly even reducing it such that
;      ; it was all fixnum arithmetic.
  (let* ((d (ash (+ ut 690882.)
		 2))
	 (r (ash (rem d 2551443.) -2)))
    (values (ldb (byte 2 0) (truncate d 2551443.))
	    (truncate r (* 60. 60. 24.))
	    (gcd (truncate r 3600.) 24.)
	    (rem(truncate r 60.) 60.)
	    (rem r 60.))))

;(defun test (&optional (ut (get-universal-time)))
;  (let* ((year (nth-value 5 (decode-universal-time ut)))
;	 (d (* (+ (- (+ (* year 365.)
;			(ash (1- year) -2))
;		     (truncate (1- year) 100.))
;		  (truncate (1- year) 400.)
;		  1)
;	       (* 60. 60. 24.))))
;    (format t "~D ~D" ut d)
;    (values ut d)))

(defun print-moonphase (quarter day hour minute second
			&optional (destination t))
  (format destination
	  "~A~@[+~:[~*~;~DD.~]~:[~*~;~DH.~]~:[~*~;~DM.~]~:[~*~;~DS.~]~]"
	  (nth quarter '("NM" "FQ" "FM" "LQ"))
	  (= (+ day hour minute second) 0)
	  (= day 0) day (= hour 0) hour
	  (= minute 0) minute (= second 0) second))


(defun print-universal-moonphase (ut &optional (destination t))
  (multiple-value-bind (quarter day hour minute second) (moonphase ut)
    (print-moonphase quarter day hour minute second destination)))

(defun print-current-moonphase (&optional (destination t))
  (print-universal-moonphase (get-universal-time) destination))
|#


;;;; Some useful strings and accessing functions.

;;; Days of the week.  Elements must be (in order):
;;; (1) Three-letter form.
;;; (2) Full spelling.
;;; (3) Middle-length form if any, else NIL.
;;; (4) Francais.
;;; (5) Deutsch.
;;; (6) Italian.  ; How do you say that in Italian ?

(DEFVAR *DAYS-OF-THE-WEEK* '(("Mon" "Monday" NIL "Lundi" "Montag" "Lunedi")
			     ("Tue" "Tuesday" "Tues" "Mardi" "Dienstag" "Martedi")
			     ("Wed" "Wednesday" NIL "Mercredi" "Mittwoch" "Mercoledi")
			     ("Thu" "Thursday" "Thurs" "Jeudi" "Donnerstag" "Giovedi")
			     ("Fri" "Friday" NIL "Vendredi" "Freitag" "Venerdi")
			     ("Sat" "Saturday" NIL "Samedi" "Samstag" "Sabato")
			     ("Sun" "Sunday" NIL "Dimanche" "Sonntag" "Domenica"))
	"The list of the days of the week in short, long, medium, French, German, Italian." )

(DEFUN DAY-OF-THE-WEEK-STRING (DAY-OF-THE-WEEK &OPTIONAL (MODE :LONG) &AUX STRINGS)
  (SETQ STRINGS (NTH DAY-OF-THE-WEEK *DAYS-OF-THE-WEEK*))
  (CASE MODE
    (:SHORT (FIRST STRINGS))
    (:LONG (SECOND STRINGS))
    (:MEDIUM (OR (THIRD STRINGS) (FIRST STRINGS)))
    (:FRENCH (FOURTH STRINGS))
    (:GERMAN (FIFTH STRINGS))
    (:ITALIAN (SIXTH STRINGS))			; After this, perhaps NDOWSS ?
    (OTHERWISE (FERROR NIL "~S is not a known day-of-the-week mode" MODE))))


;;; Months of the year:  Elements must be (in order):
;;; (1) Three-letter form.
;;; (2) Full spelling.
;;; (3) Middle-length form if any, else NIL.
;;; (4) Francais.
;;; (5) Roman numerals (used in Europe).
;;; (6) Deutsch.
;;; (7) Italian.

(DEFVAR *MONTHS* '(("Jan" "January" NIL "Janvier" "I" "Januar" "Genniao")
		   ("Feb" "February" NIL "Fevrier" "II" "Februar" "Febbraio")
		   ("Mar" "March" NIL "Mars" "III" "Maerz" "Marzo")
		   ("Apr" "April" NIL "Avril" "IV" "April" "Aprile")
		   ("May" "May" NIL "Mai" "V" "Mai" "Maggio")
		   ("Jun" "June" NIL "Juin" "VI" "Juni" "Giugno")
		   ("Jul" "July" NIL "Juillet" "VII" "Juli" "Luglio")
		   ("Aug" "August" NIL "Aout" "VIII" "August" "Agosto")
		   ("Sep" "September" "Sept" "Septembre" "IX" "September" "Settembre")
		   ("Oct" "October" NIL "Octobre" "X" "Oktober" "Ottobre")
		   ("Nov" "November" "Novem" "Novembre" "XI" "November" "Novembre")
		   ("Dec" "December" "Decem" "Decembre" "XII" "Dezember" "Dicembre"))
  "List of names lists of names of months: short, long, medium, French, Roman, German, Italian")

(DEFUN MONTH-STRING (MONTH &OPTIONAL (MODE :LONG) &AUX STRINGS)
  (SETQ STRINGS (NTH (1- MONTH) *MONTHS*))
  (CASE MODE
    (:SHORT (FIRST STRINGS))
    (:LONG (SECOND STRINGS))
    (:MEDIUM (OR (THIRD STRINGS) (FIRST STRINGS)))
    (:FRENCH (FOURTH STRINGS))
    (:ROMAN (FIFTH STRINGS))
    (:GERMAN (SIXTH STRINGS))
    (:ITALIAN (SEVENTH STRINGS))
    (OTHERWISE (FERROR NIL "~S is not a known month mode" MODE))))

(DEFVAR *TIMEZONES* '((0 "GMT" NIL #/Z)			;Greenwich
		      (0 "UT" NIL #/Z)
		      (1 NIL NIL #/A)
		      (2 NIL NIL #/B)
		      (3 NIL "ADT" #/C)
		      (4 "AST" "EDT" #/D)		;Atlantic
		      (5 "EST" "CDT" #/E)		;Eastern
		      (6 "CST" "MDT" #/F)		;Central
		      (7 "MST" "PDT" #/G)		;Mountain
		      (8 "PST" "YDT" #/H)		;Pacific
		      (9 "YST" "HDT" #/I)		;Yukon
		      (10. "HST" "BDT" #/K)		;Hawaiian
		      (11. "BST" NIL #/L)		;Bering
		      (12. NIL NIL #/M)
		      (-1 NIL NIL #/N)
		      (-2 NIL NIL #/O)
		      (-3 NIL NIL #/P)
		      (-4 NIL NIL #/Q)
		      (-5 NIL NIL #/R)
		      (-6 NIL NIL #/S)
		      (-7 NIL NIL #/T)
		      (-8 NIL NIL #/U)
		      (-9 NIL NIL #/V)
		      (-10. NIL NIL #/W)
		      (-11. NIL NIL #/X)
		      (-12. NIL NIL #/Y)
		      (3.5 "NST" NIL -1)		;Newfoundland
		      )
  "List of timezones: offset from gmt, name, daylight-savings-name, military character.")

(DEFUN TIMEZONE-STRING (&OPTIONAL (TIMEZONE *TIMEZONE*)
				  (DAYLIGHT-SAVINGS-P (DAYLIGHT-SAVINGS-P)))
  "Return a string describing timezone TIMEZONE, optionally for daylight savings time.
Defaults are our own timezone, and DST if it is now in effect."
  (IF DAYLIGHT-SAVINGS-P
      (THIRD (ASSQ (1- TIMEZONE) *TIMEZONES*))
      (SECOND (ASSQ TIMEZONE *TIMEZONES*))))

;;;; Date and time parsing

(DEFMACRO BAD-DATE-OR-TIME (REASON . ARGS)
  `(*THROW 'BAD-DATE-OR-TIME ,(IF (NULL ARGS) REASON `(FORMAT NIL ,REASON . ,ARGS))))

(DEFUN VERIFY-DATE (DAY MONTH YEAR DAY-OF-THE-WEEK)
  "If the day of the week of the date specified by DATE, MONTH, and YEAR
is the same as DAY-OF-THE-WEEK, return NIL; otherwise, return a string that
contains a suitable error message. If YEAR is less than 100, it is shifted
by centuries until it is within 50 years of the present."
  (COND ((> DAY (MONTH-LENGTH MONTH YEAR))
	 (FORMAT NIL "~A only has ~D day~:P" (MONTH-STRING MONTH) (MONTH-LENGTH MONTH YEAR)))
	(DAY-OF-THE-WEEK
	 (LET ((UT (ENCODE-UNIVERSAL-TIME 0 0 0 DAY MONTH YEAR)))
	   (MULTIPLE-VALUE-BIND (NIL NIL NIL NIL NIL NIL CORRECT-DAY-OF-THE-WEEK)
	       (DECODE-UNIVERSAL-TIME UT)
	     (AND ( DAY-OF-THE-WEEK CORRECT-DAY-OF-THE-WEEK)
		  (FORMAT NIL "The ~:R of ~A, ~D is a ~A, not a ~A"
			  (MONTH-STRING MONTH) DAY YEAR
			  (DAY-OF-THE-WEEK-STRING CORRECT-DAY-OF-THE-WEEK)
			  (DAY-OF-THE-WEEK-STRING DAY-OF-THE-WEEK))))))
	(T
	 NIL)))

(ADD-INITIALIZATION "Initialize Timebase"
  '(PROGN (SETQ LAST-BOOT-TIME (TIME) WAS-NEGATIVE NIL HIGH-TIME-BITS 0)
	  (INITIALIZE-TIMEBASE))
  '(:WARM :NOW))


;;;; This is code to read and initialize the LAMBDA's battery clock.

(defconst rtc-address-register-multibus-address #x1c124)
(defconst rtc-data-register-multibus-address #x1c120)

(defun read-rtc-reg (adr)
  (if-in-lambda
    (%nubus-write #xff rtc-address-register-multibus-address adr)
    (ldb #o0010 (%nubus-read #xff rtc-data-register-multibus-address))))

(defun write-rtc-reg (adr data)
  (if-in-lambda
    (%nubus-write #xff rtc-address-register-multibus-address adr)
    (%nubus-write #xff rtc-data-register-multibus-address data)))

;;; 0-15 used by clock chip
;;; 16,17 time zone
;;; 20 34 validation string.  "time is good"  for this version.
;;; 35 speed.  source cycles*8+execute cycles
;;; last-boot-by  0 unknown, 1 SDU dribble, 2 SDU via serial, 3 SDU via chaos, 4 CADR via debug.
;;; 36 rg-slot
;;; 37 tv-slot
;;; 40 prime-memory slot
;;; sdu serial port availability to LAMBDA. bit 1 port A, bit 2 port B.
;;; 41 ether exists.  bit 0, 3com ethernet board in usual place, bit 1 exelan board, etc.
;;; 42 magtape exists.   1 cipher streamer, 2 buffered cipher, 3 Kennedy,
;;; disk exists bitcode.  bit 0 unit 0 exists, bit 1 unit 1, etc.
;;; disk number to boot from.
;;; multibus serial card code.  0 none, 1 systec

(defvar rtc-array (IF-IN-LAMBDA (make-array 64.)))

(defconst rtc-seconds 0)
(defconst rtc-seconds-alarm 1)
(defconst rtc-minutes 2)
(defconst rtc-minutes-alarm 3)
(defconst rtc-hours 4)
(defconst rtc-hours-alram 5)
(defconst rtc-day-of-week 6)
(defconst rtc-date 7)
(defconst rtc-month #o10)
(defconst rtc-year #o11)
(defconst rtc-reg-a #o12)
(defconst rtc-reg-b #o13)
(defconst rtc-reg-c #o14)
(defconst rtc-reg-d #o15)
(defconst rtc-time-zone-low #o16)		;not maintained by chip
(defconst rtc-time-zone-hi #o17)		;not maintained by chip
;;; the rtc-cookie is written into the CMOS ram in the clock chip.  If the battery
;;; ever fails, the string will get trashed, so we wont believe the time.
(defconst rtc-cookie-start #o20)
(defconst rtc-cookie "time is good")

;;; Bits in reg-a
(defconst %%rtc-update-bit #o0701)

;;; Bits in reg-b
(defconst %%rtc-set-mode #o0701)
(defconst %%rtc-binary-mode #o0201)
(defconst %%rtc-24-hour-mode #o0101)
(defconst %%rtc-daylight-savings-enable #o0001)

;;; Bits in reg-d
(defconst %%rtc-valid-bit #o0701)

(defun read-rtc-chip ()
  (when (bit-test %%rtc-update-bit (read-rtc-reg rtc-reg-a))
    (process-sleep 2)
    (when (bit-test %%rtc-update-bit (read-rtc-reg rtc-reg-a))
      (format *error-output* "~&Warning: update bit on clock chip seems to be stuck.")))
  (dotimes (i 64.)
    (aset (read-rtc-reg i) rtc-array i)))

(defun write-rtc-chip ()
  (dotimes (i 64.)
    (write-rtc-reg i (aref rtc-array i))))

;(defun print-rtc ()
;  (read-rtc-chip)
;  (dotimes (i 64.)
;    (if (zerop (logand i 7)) (fresh-line))
;    (format t "~16,2r " (aref rtc-array i))))

;;;the chip turns off this bit if the battery ever dies
;;; this would be nice, but the SDU touches it before we get a chance to ...
;(defun rtc-valid-p ()
;  (bit-test %%rtc-valid-bit (read-rtc-reg rtc-reg-d)))

(defun rtc-valid-p ()
  (let ((s (with-output-to-string (str)
	     (dotimes (i (string-length rtc-cookie))
	       (send str :tyo (aref rtc-array (+ rtc-cookie-start i)))))))
    (string= s rtc-cookie)))

(defvar lambda-number-of-source-cycles nil)
(defvar lambda-number-of-execute-cycles nil)
(defvar lambda-rg-slot-number nil)
(defvar lambda-tv-slot-number nil)
(defvar lambda-prime-memory-slot-number nil)
(defvar lambda-ethernet-configuration-mask nil)
(defvar lambda-magtape-configuration-mask nil)

(defun get-lambda-configuration-from-rtc-chip ()
  (unless (rtc-valid-p)
    (ferror nil "cmos ram configuration information not set up"))
  (read-rtc-chip)
  (setq lambda-number-of-source-cycles (ldb #o0303 (aref rtc-array #o35)))
  (setq lambda-number-of-execute-cycles (ldb #o0003 (aref rtc-array #o35)))
  (setq lambda-rg-slot-number (aref rtc-array #o36))
  (setq lambda-tv-slot-number (aref rtc-array #o37))
  (setq lambda-prime-memory-slot-number (aref rtc-array #o40))
  (setq lambda-ethernet-configuration-mask (aref rtc-array #o41))
  (setq lambda-magtape-configuration-mask (aref rtc-array #o42))
  (WHEN (or (not (member lambda-number-of-source-cycles '(1 2 3)))
	    (not (member lambda-number-of-execute-cycles '(1 2 3)))
	    (< lambda-rg-slot-number 0)
	    (> lambda-rg-slot-number #o14)
	    (< lambda-tv-slot-number 0)
	    (> lambda-tv-slot-number #o14)
	    (< lambda-prime-memory-slot-number 0)
	    (> lambda-prime-memory-slot-number #o14))
    (ferror nil "bad data in cmos configuration ram")))

(defun rtc-get-universal-time ()
  (read-rtc-chip)
  (cond ((rtc-valid-p)
	 (let ((tz (// (dpb (aref rtc-array rtc-time-zone-hi)
			    #o1010
			    (aref rtc-array rtc-time-zone-low))
		       60.)))
	   (unless (= tz time:*timezone*)
	     (format *error-output* "~&warning: timezone in rtc chip is wrong")))
	 (time:encode-universal-time
	   (aref rtc-array rtc-seconds)
	   (aref rtc-array rtc-minutes)
	   (aref rtc-array rtc-hours)
	   (aref rtc-array rtc-date)
	   (aref rtc-array rtc-month)
	   (+ (aref rtc-array rtc-year) 1900.)))
	(t nil)))

;(defun print-rtc-array ()
;  (format t "~d//~d//~d ~d:~d:~d"
;	  (aref rtc-array rtc-month)
;	  (aref rtc-array rtc-date)
;	  (aref rtc-array rtc-year)
;	  (aref rtc-array rtc-hours)
;	  (aref rtc-array rtc-minutes)
;	  (aref rtc-array rtc-seconds)))

(defun rtc-set-universal-time (ut)
  (read-rtc-chip)
  (write-rtc-reg rtc-reg-b (logior (dpb 1 %%rtc-set-mode 0)
				   (dpb 1 %%rtc-binary-mode 0)
				   (dpb 1 %%rtc-24-hour-mode 0)
				   (dpb 1 %%rtc-daylight-savings-enable 0)))
  (multiple-value-bind (seconds minutes hours date month year day-of-week daylight-savings-p)
      (time:decode-universal-time ut)
    (aset seconds rtc-array rtc-seconds)
    (aset minutes rtc-array rtc-minutes)
    (aset (if daylight-savings-p
	      (1+ hours)
	    hours)
	  rtc-array rtc-hours)
    (aset date rtc-array rtc-date)
    (aset month rtc-array rtc-month)
    (aset (- year 1900.) rtc-array rtc-year)
    (aset (if (= day-of-week 6)
	      1
	    (+ day-of-week 2)) rtc-array rtc-day-of-week))
  (aset (ldb #o0010 (* time:*timezone* 60.)) rtc-array rtc-time-zone-low)
  (aset (ldb #o1010 (* time:*timezone* 60.)) rtc-array rtc-time-zone-hi)
  (dotimes (i (string-length rtc-cookie))
    (aset (aref rtc-cookie i) rtc-array (+ i rtc-cookie-start)))
  (write-rtc-chip)
  ;;start time ticking
  (write-rtc-reg rtc-reg-b (logior (dpb 1 %%rtc-binary-mode 0)
				   (dpb 1 %%rtc-24-hour-mode 0)
				   (dpb 1 %%rtc-daylight-savings-enable 0)))
; (read-rtc-reg rtc-reg-d) ; reading this reg sets the valid bit
  t)
