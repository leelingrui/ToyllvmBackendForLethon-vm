; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc < %s -mtriple=s390x-linux-gnu -mcpu=z15 | FileCheck %s
;
; Test that DAGCombiner does not change the addressing as the displacements
; are known to be out of range. Only one addition is needed.

define void @fun(<2 x i64>* %Src, <2 x i64>* %Dst) {
; CHECK-LABEL: fun:
; CHECK:       # %bb.0:
; CHECK-NEXT:    aghi %r2, 4096
; CHECK-NEXT:    vl %v0, 0(%r2), 3
; CHECK-NEXT:    vst %v0, 0(%r3), 3
; CHECK-NEXT:    vl %v0, 16(%r2), 3
; CHECK-NEXT:    vst %v0, 0(%r3), 3
; CHECK-NEXT:    br %r14
  %1 = bitcast <2 x i64>* %Src to i8*

  %splitgep = getelementptr i8, i8* %1, i64 4096
  %2 = bitcast i8* %splitgep to <2 x i64>*
  %V0 = load <2 x i64>, <2 x i64>* %2, align 8
  store volatile <2 x i64> %V0, <2 x i64>* %Dst, align 8

  %3 = getelementptr i8, i8* %splitgep, i64 16
  %4 = bitcast i8* %3 to <2 x i64>*
  %V1 = load <2 x i64>, <2 x i64>* %4, align 8
  store volatile <2 x i64> %V1, <2 x i64>* %Dst, align 8

  ret void
}