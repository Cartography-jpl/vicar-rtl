      SUBROUTINE DROTMG (D1,D2,X1,Y1,PARAM)
C>> 1994-10-20 DROTMG Krogh  Changes to use M77CON
C>> 1994-04-19 DROTMG Krogh   Converted to use generic intrinsics.
C>> 1985-08-02 DROTMG Lawson  Initial code.
c--D replaces "?": ?ROTMG
C
C     CONSTRUCT THE MODIFIED GIVENS TRANSFORMATION MATRIX H WHICH ZEROS
C     THE SECOND COMPONENT OF THE 2-VECTOR  (SQRT(D1)*X1,SQRT(D2)*
C     Y1)**T.
C     WITH PARAM(1)=PFLAG, H HAS ONE OF THE FOLLOWING FORMS..
C
C     PFLAG=-1.D0     PFLAG=0.D0        PFLAG=1.D0     PFLAG=-2.D0
C
C       (H11  H12)    (1.D0  H12)    (H11  1.D0)    (1.D0  0.D0)
C     H=(        )    (         )    (         )    (          )
C       (H21  H22),   (H21  1.D0),   (-1.D0 H22),   (0.D0  1.D0).
C     LOCATIONS 2-4 OF PARAM CONTAIN H11, H21, H12, AND H22
C     RESPECTIVELY. (VALUES OF 1.D0, -1.D0, OR 0.D0 IMPLIED BY THE
C     VALUE OF PARAM(1) ARE NOT STORED IN PARAM.)
C
C     THE VALUES OF GAMSQ AND RGAMSQ SET IN THE DATA STATEMENT MAY BE
C     INEXACT.  THIS IS OK AS THEY ARE ONLY USED FOR TESTING THE SIZE
C     OF D1 AND D2.  ALL ACTUAL SCALING OF DATA IS DONE USING GAM.
C
      DOUBLE PRECISION GAM,ONE,RGAMSQ,D2,H11,H21,PARAM,P2,
     1 Q2,U,Y1,ZERO,GAMSQ,D1,PFLAG,H12,H22,P1,Q1,
     2 TEMP,X1,TWO
      DIMENSION PARAM(5)
C
      DATA ZERO,ONE,TWO /0.D0,1.D0,2.D0/
      DATA GAM,GAMSQ,RGAMSQ/4096.D0,16777216.D0,5.9604645D-8/
      IF(.NOT. D1 .LT. ZERO) GO TO 10
C       GO ZERO-H-D-AND-X1..
          GO TO 60
   10 CONTINUE
C     CASE-D1-NONNEGATIVE
      P2=D2*Y1
      IF(.NOT. P2 .EQ. ZERO) GO TO 20
          PFLAG=-TWO
          GO TO 260
C     REGULAR-CASE..
   20 CONTINUE
C     PRINT*,'D1,X1 =',D1,X1
      P1=D1*X1
      Q2=P2*Y1
      Q1=P1*X1
C
      IF(.NOT. ABS(Q1) .GT. ABS(Q2)) GO TO 40
          H21=-Y1/X1
          H12=P2/P1
C
          U=ONE-H12*H21
C
          IF(.NOT. U .LE. ZERO) GO TO 30
C         GO ZERO-H-D-AND-X1..
               GO TO 60
   30     CONTINUE
               PFLAG=ZERO
               D1=D1/U
               D2=D2/U
               X1=X1*U
C         GO SCALE-CHECK..
               GO TO 100
   40 CONTINUE
          IF(.NOT. Q2 .LT. ZERO) GO TO 50
C         GO ZERO-H-D-AND-X1..
               GO TO 60
   50     CONTINUE
               PFLAG=ONE
               H11=P1/P2
               H22=X1/Y1
               U=ONE+H11*H22
               TEMP=D2/U
               D2=D1/U
               D1=TEMP
               X1=Y1*U
C         GO SCALE-CHECK
               GO TO 100
C     PROCEDURE..ZERO-H-D-AND-X1..
   60 CONTINUE
          PFLAG=-ONE
          H11=ZERO
          H12=ZERO
          H21=ZERO
          H22=ZERO
C
          D1=ZERO
          D2=ZERO
          X1=ZERO
C         RETURN..
          GO TO 220
C     PROCEDURE..FIX-H..
   70 CONTINUE
      IF(.NOT. PFLAG .GE. ZERO) GO TO 90
C
          IF(.NOT. PFLAG .EQ. ZERO) GO TO 80
          H11=ONE
          H22=ONE
          PFLAG=-ONE
          GO TO 90
   80     CONTINUE
          H21=-ONE
          H12=ONE
          PFLAG=-ONE
   90 CONTINUE
      GO TO IGO,(120,150,180,210)
C     PROCEDURE..SCALE-CHECK
  100 CONTINUE
  110     CONTINUE
          IF(.NOT. D1 .LE. RGAMSQ) GO TO 130
               IF(D1 .EQ. ZERO) GO TO 160
               ASSIGN 120 TO IGO
C              FIX-H..
               GO TO 70
  120          CONTINUE
               D1=D1*GAM**2
               X1=X1/GAM
               H11=H11/GAM
               H12=H12/GAM
          GO TO 110
  130 CONTINUE
  140     CONTINUE
          IF(.NOT. D1 .GE. GAMSQ) GO TO 160
               ASSIGN 150 TO IGO
C              FIX-H..
               GO TO 70
  150          CONTINUE
               D1=D1/GAM**2
               X1=X1*GAM
               H11=H11*GAM
               H12=H12*GAM
          GO TO 140
  160 CONTINUE
  170     CONTINUE
          IF(.NOT. ABS(D2) .LE. RGAMSQ) GO TO 190
               IF(D2 .EQ. ZERO) GO TO 220
               ASSIGN 180 TO IGO
C              FIX-H..
               GO TO 70
  180          CONTINUE
               D2=D2*GAM**2
               H21=H21/GAM
               H22=H22/GAM
          GO TO 170
  190 CONTINUE
  200     CONTINUE
          IF(.NOT. ABS(D2) .GE. GAMSQ) GO TO 220
               ASSIGN 210 TO IGO
C              FIX-H..
               GO TO 70
  210          CONTINUE
               D2=D2/GAM**2
               H21=H21*GAM
               H22=H22*GAM
          GO TO 200
  220 CONTINUE
          IF(PFLAG)250,230,240
  230     CONTINUE
               PARAM(3)=H21
               PARAM(4)=H12
               GO TO 260
  240     CONTINUE
               PARAM(2)=H11
               PARAM(5)=H22
               GO TO 260
  250     CONTINUE
               PARAM(2)=H11
               PARAM(3)=H21
               PARAM(4)=H12
               PARAM(5)=H22
  260 CONTINUE
          PARAM(1)=PFLAG
          RETURN
      END
