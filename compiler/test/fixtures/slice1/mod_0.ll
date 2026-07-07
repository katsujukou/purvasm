; ModuleID = 'purvasm.module'

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






@pv_g_Slice1_2eidentInt$root = global i64 0

define void @pv_g_Slice1_2eidentInt$init(ptr %ctx) {
entry:
  %t1 = ptrtoint ptr @pv_g_Slice1_2eidentInt to i64
  %t2 = call i64 @pv_make_closure(ptr %ctx, i64 %t1, i32 1, i64 1)
  br label %rchk1
rchk1:
  %t3 = getelementptr i8, ptr %ctx, i64 8
  %t4 = load i64, ptr %t3
  %t6 = getelementptr i8, ptr %ctx, i64 16
  %t5 = load i64, ptr %t6
  %t7 = icmp eq i64 %t4, %t5
  br i1 %t7, label %rslow3, label %rfast2
rfast2:
  %t8 = load ptr, ptr %ctx
  %t9 = getelementptr i64, ptr %t8, i64 %t4
  store i64 %t2, ptr %t9
  %t10 = add i64 %t4, 1
  store i64 %t10, ptr %t3
  br label %rdone4
rslow3:
  %t11 = call i64 @pv_root(ptr %ctx, i64 %t2)
  br label %rdone4
rdone4:
  %t12 = phi i64 [ %t4, %rfast2 ], [ %t11, %rslow3 ]
  store i64 %t12, ptr @pv_g_Slice1_2eidentInt$root
  ret void
}

define tailcc i64 @pv_g_Slice1_2eidentInt$d(ptr %ctx, i64 %env, i64 %p0) {
entry:
  %t2 = getelementptr i8, ptr %ctx, i64 8
  %t1 = load i64, ptr %t2
  br label %rchk5
rchk5:
  %t3 = getelementptr i8, ptr %ctx, i64 8
  %t4 = load i64, ptr %t3
  %t6 = getelementptr i8, ptr %ctx, i64 16
  %t5 = load i64, ptr %t6
  %t7 = icmp eq i64 %t4, %t5
  br i1 %t7, label %rslow7, label %rfast6
rfast6:
  %t8 = load ptr, ptr %ctx
  %t9 = getelementptr i64, ptr %t8, i64 %t4
  store i64 %p0, ptr %t9
  %t10 = add i64 %t4, 1
  store i64 %t10, ptr %t3
  br label %rdone8
rslow7:
  %t11 = call i64 @pv_root(ptr %ctx, i64 %p0)
  br label %rdone8
rdone8:
  %t12 = phi i64 [ %t4, %rfast6 ], [ %t11, %rslow7 ]
  %t13 = load ptr, ptr %ctx
  %t14 = getelementptr i64, ptr %t13, i64 %t12
  %t15 = load i64, ptr %t14
  %t16 = getelementptr i8, ptr %ctx, i64 8
  store i64 %t1, ptr %t16
  ret i64 %t15
}

define internal i64 @pv_g_Slice1_2eidentInt(ptr %ctx, i64 %clo, ptr %args, i64 %nargs) {
entry:
  %t1 = getelementptr i64, ptr %args, i64 0
  %t2 = load i64, ptr %t1
  %t3 = call tailcc i64 @pv_g_Slice1_2eidentInt$d(ptr %ctx, i64 1, i64 %t2)
  ret i64 %t3
}

