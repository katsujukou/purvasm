; ModuleID = 'purvasm.init'

declare ptr @pv_runtime_new(i64)
declare void @pv_abi_check(i32)
declare void @pv_runtime_free(ptr)
declare i64 @pv_apply(ptr, i64, ptr, i64)
declare void @pv_tailcall(ptr, i64, ptr, i64)
declare i64 @pv_settle(ptr, i64)
declare i64 @pv_make_closure(ptr, i64, i32, i64)
declare i64 @pv_frame(ptr)
declare i64 @pv_root(ptr, i64)
declare i64 @pv_get(ptr, i64)
declare void @pv_pop_frame(ptr, i64)
declare i64 @pv_new_array(ptr, ptr, i64)
declare i64 @pv_new_adt(ptr, i32, ptr, i64)
declare i64 @pv_new_record(ptr, ptr, ptr, i64)
declare i64 @pv_new_str(ptr, ptr, i64)
declare i64 @pv_new_number(ptr, i64)
declare i64 @pv_record_get(ptr, i64, i64)
declare i64 @pv_record_set(ptr, i64, i64, i64)
declare i64 @pv_read_field(ptr, i64, i64)
declare void @pv_write_field(ptr, i64, i64, i64)
declare i64 @pv_read_raw(ptr, i64, i64)
declare void @pv_case_fail()
declare i64 @pv_run_effect(ptr, i64)
declare void @pv_drain_output(ptr)
declare void @pv_print_int(i64)
declare i64 @pv_prim_add_int(i64, i64)
declare i64 @pv_prim_sub_int(i64, i64)
declare i64 @pv_prim_mul_int(i64, i64)
declare i64 @pv_prim_div_int(i64, i64)
declare i64 @pv_prim_mod_int(i64, i64)
declare i64 @pv_prim_and_int(i64, i64)
declare i64 @pv_prim_or_int(i64, i64)
declare i64 @pv_prim_xor_int(i64, i64)
declare i64 @pv_prim_shl_int(i64, i64)
declare i64 @pv_prim_shr_int(i64, i64)
declare i64 @pv_prim_zshr_int(i64, i64)
declare i64 @pv_prim_complement_int(i64)
declare i64 @pv_prim_eq_int(i64, i64)
declare i64 @pv_prim_lt_int(i64, i64)
declare i64 @pv_prim_eq_bool(i64, i64)
declare i64 @pv_prim_and_bool(i64, i64)
declare i64 @pv_prim_or_bool(i64, i64)
declare i64 @pv_prim_not_bool(i64)
declare i64 @pv_prim_add_number(ptr, i64, i64)
declare i64 @pv_prim_sub_number(ptr, i64, i64)
declare i64 @pv_prim_mul_number(ptr, i64, i64)
declare i64 @pv_prim_div_number(ptr, i64, i64)
declare i64 @pv_prim_int_to_number(ptr, i64)
declare i64 @pv_prim_number_to_int(ptr, i64)
declare i64 @pv_prim_eq_number(ptr, i64, i64)
declare i64 @pv_prim_lt_number(ptr, i64, i64)
declare i64 @pv_prim_eq_string(ptr, i64, i64)
declare i64 @pv_prim_lt_string(ptr, i64, i64)
declare i64 @pv_prim_append(ptr, i64, i64)
declare i64 @pv_prim_index_array(ptr, i64, i64)
declare i64 @pv_prim_length_array(ptr, i64)
declare i64 @pv_prim_new_array(ptr, i64)
declare i64 @pv_prim_set_array(ptr, i64, i64, i64)
declare i64 @pv_prim_record_get(ptr, i64, i64)
declare i64 @pv_prim_record_set(ptr, i64, i64, i64)
declare i64 @pv_prim_record_has(ptr, i64, i64)
declare i64 @pv_prim_record_delete(ptr, i64, i64)
declare i64 @pv_prim_record_union(ptr, i64, i64)
declare i64 @pv_empty_array()
declare i64 @pv_new_byneed_placeholder(ptr)
declare void @pv_byneed_set_suspension(ptr, i64, i64)
declare i64 @pv_force_if_byneed(ptr, i64)
@pv_ctx_abi_v1 = external global i8
@pv_abi_stamp = internal constant ptr @pv_ctx_abi_v1
@llvm.used = appending global [1 x ptr] [ptr @pv_abi_stamp], section "llvm.metadata"

declare void @pv_g_Slice1_2eidentInt$init(ptr)
@pv_g_Slice1_2eidentInt$root = external global i64




define void @pv_init_all(ptr %ctx) {
entry:
  call void @pv_g_Slice1_2eidentInt$init(ptr %ctx)
  ret void
}


define i32 @main() {
entry:
  %ctx = call ptr @pv_runtime_new(i64 1048576)
  call void @pv_abi_check(i32 1)
  call void @pv_init_all(ptr %ctx)
  %t2 = getelementptr i8, ptr %ctx, i64 8
  %t1 = load i64, ptr %t2
  %t3 = load i64, ptr @pv_g_Slice1_2eidentInt$root
  %t4 = load ptr, ptr %ctx
  %t5 = getelementptr i64, ptr %t4, i64 %t3
  %t6 = load i64, ptr %t5
  br label %fchk1
fchk1:
  %t7 = and i64 %t6, 1
  %t8 = icmp ne i64 %t7, 0
  br i1 %t8, label %fdone3, label %fslow2
fslow2:
  %t9 = call i64 @pv_force_if_byneed(ptr %ctx, i64 %t6)
  br label %fdone3
fdone3:
  %t10 = phi i64 [ %t6, %fchk1 ], [ %t9, %fslow2 ]
  call void @pv_print_int(i64 %t10)
  %t11 = getelementptr i8, ptr %ctx, i64 8
  store i64 %t1, ptr %t11
  call void @pv_runtime_free(ptr %ctx)
  ret i32 0
}
