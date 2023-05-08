mod infra;

success_tests! {
	{
		name: nested_arith1_1,
		file: "nested_arith1.snek",
		expected: "25",
	},
	{
		name: binding_nested_1,
		file: "binding_nested.snek",
		expected: "1",
	},
	{
		name: shadowed_binding_succ1_1,
		file: "shadowed_binding_succ1.snek",
		expected: "7",
	},
	{
		name: set_expr3_4,
		file: "set_expr3.snek",
		input: "25",
		expected: "true",
	},
	{
		name: set_expr3_5,
		file: "set_expr3.snek",
		input: "20",
		expected: "false",
	},
	{
		name: if_expr_succ2_1,
		file: "if_expr_succ2.snek",
		expected: "8",
	},
	{
		name: func_fact_1_2,
		file: "func_fact_1.snek",
		expected: "120",
	},
	{
		name: func_fact_1_3,
		file: "func_fact_1.snek",
		input: "20",
		expected: "2432902008176640000",
	},
	{
		name: if_expr_succ3_1,
		file: "if_expr_succ3.snek",
		expected: "7",
	},
	{
		name: set_expr2_5,
		file: "set_expr2.snek",
		expected: "25",
	},
	{
		name: add1_1,
		file: "add1.snek",
		expected: "73",
	},
	{
		name: num_1,
		file: "num.snek",
		expected: "644",
	},
	{
		name: shadowed_binding_succ0_1,
		file: "shadowed_binding_succ0.snek",
		expected: "100",
	},
	{
		name: type_check_succ5_1,
		file: "type_check_succ5.snek",
		expected: "true",
	},
	{
		name: nested_arith0_1,
		file: "nested_arith0.snek",
		expected: "35",
	},
	{
		name: shadowed_binding_succ7_1,
		file: "shadowed_binding_succ7.snek",
		expected: "200",
	},
	{
		name: print1_2,
		file: "print1.snek",
		input: "5",
		expected: "2\n5",
	},
	{
		name: print1_3,
		file: "print1.snek",
		input: "-3",
		expected: "-6\n-3",
	},
	{
		name: fact_15,
		file: "fact.snek",
		input: "10",
		expected: "3628800",
	},
	{
		name: func_fib_2,
		file: "func_fib.snek",
		expected: "55",
	},
	{
		name: binding1_1,
		file: "binding1.snek",
		expected: "-5",
	},
	{
		name: binding0_1,
		file: "binding0.snek",
		expected: "5",
	},
	{
		name: compare_expr_succ2_1,
		file: "compare_expr_succ2.snek",
		expected: "true",
	},
	{
		name: isnum_1,
		file: "isnum.snek",
		expected: "false",
	},
	{
		name: isnum_2,
		file: "isnum.snek",
		input: "547",
		expected: "true",
	},
	{
		name: isnum_3,
		file: "isnum.snek",
		input: "true",
		expected: "false",
	},
	{
		name: shadowed_binding_succ6_1,
		file: "shadowed_binding_succ6.snek",
		expected: "3",
	},
	{
		name: complex_expr_1,
		file: "complex_expr.snek",
		expected: "6",
	},
	{
		name: factr_chain_4,
		file: "factr_chain.snek",
		input: "5",
		expected: "240",
	},
	{
		name: shadowed_binding_succ5_1,
		file: "shadowed_binding_succ5.snek",
		expected: "5",
	},
	{
		name: func_many_args_3,
		file: "func_many_args.snek",
		expected: "15\n22\n351\n351",
	},
	{
		name: even_odd_19,
		file: "even_odd.snek",
		input: "10",
		expected: "10\ntrue\ntrue",
	},
	{
		name: even_odd_20,
		file: "even_odd.snek",
		input: "9",
		expected: "9\nfalse\nfalse",
	},
	{
		name: if_expr_input_1,
		file: "if_expr_input.snek",
		input: "635",
		expected: "20",
	},
	{
		name: func_nested_call_11,
		file: "func_nested_call.snek",
		expected: "15\n15\n30",
	},
	{
		name: isbool_1,
		file: "isbool.snek",
		expected: "true",
	},
	{
		name: isbool_2,
		file: "isbool.snek",
		input: "689",
		expected: "false",
	},
	{
		name: nested_arith4_1,
		file: "nested_arith4.snek",
		expected: "-1",
	},
	{
		name: factr_2,
		file: "factr.snek",
		input: "10",
		expected: "1\n1\n2\n2\n3\n6\n4\n24\n5\n120\n6\n720\n7\n5040\n8\n40320\n9\n362880\n10\n3628800",
	},
	{
		name: shadowed_binding_succ4_1,
		file: "shadowed_binding_succ4.snek",
		expected: "18",
	},
	{
		name: binding_chain_1,
		file: "binding_chain.snek",
		expected: "3",
	},
	{
		name: func_no_args_2,
		file: "func_no_args.snek",
		expected: "5\n5",
	},
	{
		name: binding_nested_chain_1,
		file: "binding_nested_chain.snek",
		expected: "12",
	},
	{
		name: compare_expr_succ0_1,
		file: "compare_expr_succ0.snek",
		expected: "true",
	},
	{
		name: false_val_1,
		file: "false_val.snek",
		expected: "false",
	},
	{
		name: binding_expr_1,
		file: "binding_expr.snek",
		expected: "1225",
	},
	{
		name: nested_arith3_1,
		file: "nested_arith3.snek",
		input: "8",
		expected: "1117",
	},
	{
		name: if_expr_succ0_1,
		file: "if_expr_succ0.snek",
		expected: "10",
	},
	{
		name: set_expr1_1,
		file: "set_expr1.snek",
		expected: "true",
	},
	{
		name: loop_expr1_17,
		file: "loop_expr1.snek",
		expected: "-6",
	},
	{
		name: shadowed_binding_succ3_1,
		file: "shadowed_binding_succ3.snek",
		expected: "5",
	},
	{
		name: shadowed_binding_succ2_1,
		file: "shadowed_binding_succ2.snek",
		expected: "150",
	},
	{
		name: input0_1,
		file: "input0.snek",
		expected: "false",
	},
	{
		name: input0_2,
		file: "input0.snek",
		input: "true",
		expected: "true",
	},
	{
		name: input0_3,
		file: "input0.snek",
		input: "123",
		expected: "123",
	},
	{
		name: loop_expr0_10,
		file: "loop_expr0.snek",
		input: "3",
		expected: "6",
	},
	{
		name: loop_expr0_11,
		file: "loop_expr0.snek",
		input: "7",
		expected: "5040",
	},
	{
		name: add_1,
		file: "add.snek",
		input: "10",
		expected: "15",
	},
	{
		name: quick_brown_fox_1,
		file: "quick_brown_fox.snek",
		expected: "-3776",
	},
	{
		name: add1_sub1_1,
		file: "add1_sub1.snek",
		expected: "4",
	},
	{
		name: nested_arith2_1,
		file: "nested_arith2.snek",
		expected: "0",
	},
}

static_error_tests! {
	{
		name: unbound_identifier_fail2_1,
		file: "unbound_identifier_fail2.snek",
		expected: "Unbound variable identifier x",
	},
	{
		name: parse_block_fail0_1,
		file: "parse_block_fail0.snek",
		expected: "Invalid",
	},
	{
		name: parse_let_improperargs_fail4_1,
		file: "parse_let_improperargs_fail4.snek",
		expected: "Invalid",
	},
	{
		name: invalid_loop_fail0_1,
		file: "invalid_loop_fail0.snek",
		expected: "Invalid",
	},
	{
		name: parse_let_improperargs_fail5_1,
		file: "parse_let_improperargs_fail5.snek",
		expected: "keyword",
	},
	{
		name: unbound_identifier_fail3_1,
		file: "unbound_identifier_fail3.snek",
		expected: "Unbound variable identifier z",
	},
	{
		name: parse_token_fail1_1,
		file: "parse_token_fail1.snek",
		expected: "Invalid",
	},
	{
		name: parse_op_fail1_1,
		file: "parse_op_fail1.snek",
		expected: "Invalid",
	},
	{
		name: parse_let_improperargs_fail2_1,
		file: "parse_let_improperargs_fail2.snek",
		expected: "Invalid",
	},
	{
		name: parse_op_fail6_1,
		file: "parse_op_fail6.snek",
		expected: "Invalid",
	},
	{
		name: parse_if_fail1_1,
		file: "parse_if_fail1.snek",
		expected: "Invalid",
	},
	{
		name: parse_sexp_fail1_1,
		file: "parse_sexp_fail1.snek",
		expected: "Invalid",
	},
	{
		name: unbound_identifier_fail4_1,
		file: "unbound_identifier_fail4.snek",
		expected: "Unbound variable identifier t",
	},
	{
		name: number_bounds_fail0_1,
		file: "number_bounds_fail0.snek",
		expected: "Invalid",
	},
	{
		name: unbound_identifier_fail5_1,
		file: "unbound_identifier_fail5.snek",
		expected: "Unbound variable identifier x",
	},
	{
		name: number_bounds_fail1_1,
		file: "number_bounds_fail1.snek",
		expected: "Invalid",
	},
	{
		name: parse_if_fail0_1,
		file: "parse_if_fail0.snek",
		expected: "Invalid",
	},
	{
		name: parse_let_nobindings_fail_1,
		file: "parse_let_nobindings_fail.snek",
		expected: "Invalid",
	},
	{
		name: parse_op_fail7_1,
		file: "parse_op_fail7.snek",
		expected: "Invalid",
	},
	{
		name: duplicate_binding_fail2_1,
		file: "duplicate_binding_fail2.snek",
		expected: "Duplicate binding",
	},
	{
		name: parse_let_improperargs_fail3_1,
		file: "parse_let_improperargs_fail3.snek",
		expected: "Invalid",
	},
	{
		name: func_dup_3,
		file: "func_dup.snek",
		expected: "",
	},
	{
		name: invalid_break_fail0_1,
		file: "invalid_break_fail0.snek",
		expected: "break",
	},
	{
		name: duplicate_binding_fail1_1,
		file: "duplicate_binding_fail1.snek",
		expected: "Duplicate binding",
	},
	{
		name: parse_op_fail8_1,
		file: "parse_op_fail8.snek",
		expected: "Invalid",
	},
	{
		name: parse_token_fail4_1,
		file: "parse_token_fail4.snek",
		expected: "Invalid",
	},
	{
		name: parse_op_fail4_1,
		file: "parse_op_fail4.snek",
		expected: "Invalid",
	},
	{
		name: parse_op_fail5_1,
		file: "parse_op_fail5.snek",
		expected: "Invalid",
	},
	{
		name: parse_sexp_fail2_1,
		file: "parse_sexp_fail2.snek",
		expected: "Invalid",
	},
	{
		name: parse_let_improperargs_fail1_1,
		file: "parse_let_improperargs_fail1.snek",
		expected: "Invalid",
	},
	{
		name: duplicate_binding_fail0_1,
		file: "duplicate_binding_fail0.snek",
		expected: "Duplicate binding",
	},
	{
		name: unbound_identifier_fail0_1,
		file: "unbound_identifier_fail0.snek",
		expected: "Unbound variable identifier x",
	},
	{
		name: parse_token_fail2_1,
		file: "parse_token_fail2.snek",
		expected: "Invalid",
	},
	{
		name: func_no_fun_2,
		file: "func_no_fun.snek",
		expected: "Invalid",
	},
	{
		name: parse_op_fail2_1,
		file: "parse_op_fail2.snek",
		expected: "Invalid",
	},
	{
		name: parse_let_improperargs_fail6_1,
		file: "parse_let_improperargs_fail6.snek",
		expected: "keyword",
	},
	{
		name: duplicate_params_5,
		file: "duplicate_params.snek",
		expected: "",
	},
	{
		name: parse_op_fail3_1,
		file: "parse_op_fail3.snek",
		expected: "Invalid",
	},
	{
		name: parse_token_fail3_1,
		file: "parse_token_fail3.snek",
		expected: "Invalid",
	},
	{
		name: unbound_identifier_fail1_1,
		file: "unbound_identifier_fail1.snek",
		expected: "Unbound variable identifier y",
	},
}

runtime_error_tests! {
	{
		name: set_expr3_6,
		file: "set_expr3.snek",
		input: "true",
		expected: "invalid argument",
	},
	{
		name: invalid_argument_fail3_1,
		file: "invalid_argument_fail3.snek",
		expected: "invalid argument",
	},
	{
		name: invalid_argument_fail2_1,
		file: "invalid_argument_fail2.snek",
		expected: "invalid argument",
	},
	{
		name: print1_1,
		file: "print1.snek",
		expected: "invalid argument",
	},
	{
		name: invalid_argument_fail5_1,
		file: "invalid_argument_fail5.snek",
		expected: "invalid argument",
	},
	{
		name: number_overflow_fail0_1,
		file: "number_overflow_fail0.snek",
		expected: "overflow",
	},
	{
		name: number_overflow_fail1_1,
		file: "number_overflow_fail1.snek",
		expected: "overflow",
	},
	{
		name: invalid_argument_fail4_1,
		file: "invalid_argument_fail4.snek",
		expected: "invalid argument",
	},
	{
		name: factr_chain_3,
		file: "factr_chain.snek",
		expected: "invalid argument",
	},
	{
		name: factr_chain_5,
		file: "factr_chain.snek",
		input: "20",
		expected: "overflow",
	},
	{
		name: if_expr_input_2,
		file: "if_expr_input.snek",
		input: "665",
		expected: "invalid argument",
	},
	{
		name: invalid_argument_fail6_1,
		file: "invalid_argument_fail6.snek",
		expected: "invalid argument",
	},
	{
		name: nested_arith3_2,
		file: "nested_arith3.snek",
		input: "4611686018427387890",
		expected: "overflow",
	},
	{
		name: nested_arith3_3,
		file: "nested_arith3.snek",
		input: "true",
		expected: "invalid argument",
	},
	{
		name: invalid_argument_fail1_1,
		file: "invalid_argument_fail1.snek",
		expected: "invalid argument",
	},
	{
		name: loop_expr0_12,
		file: "loop_expr0.snek",
		input: "5",
		expected: "invalid argument",
	},
	{
		name: add_2,
		file: "add.snek",
		input: "4611686018427387899",
		expected: "overflow",
	},
	{
		name: invalid_argument_fail0_1,
		file: "invalid_argument_fail0.snek",
		expected: "invalid argument",
	},
	{
		name: invalid_argument_fail11_1,
		file: "invalid_argument_fail11.snek",
		expected: "invalid argument",
	},
}

