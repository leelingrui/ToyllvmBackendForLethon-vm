; NOTE: Assertions have been autogenerated by utils/update_llc_test_checks.py
; RUN: llc < %s -mtriple=x86_64-linux -mattr=+avx | FileCheck %s

; TODO: We If we stored ymm0 before ymm1, then we could execute 2nd load and 1st store in
; parallel.
define void @test_01(ptr %src, ptr %dest) {
; CHECK-LABEL: test_01:
; CHECK:       # %bb.0: # %entry
; CHECK-NEXT:    vmovups (%rdi), %ymm0
; CHECK-NEXT:    vmovups 32(%rdi), %ymm1
; CHECK-NEXT:    vmovups %ymm1, 32(%rsi)
; CHECK-NEXT:    vmovups %ymm0, (%rsi)
; CHECK-NEXT:    vzeroupper
; CHECK-NEXT:    retq
entry:
  %value = load <64 x i8>, ptr %src, align 1
  store <64 x i8> %value, ptr %dest, align 1
  ret void
}