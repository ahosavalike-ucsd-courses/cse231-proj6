mod infra;

success_compile_tests! {
	{
		name: profile_bigloop,
        file: "bigloop.snek",
        input: "100000000",
        expected: "100",
	},
	{
		name: profile_factorial,
		file: "func_fact_tail.snek",
		input: "20",
		expected: "2432902008176640000",
	},
	{
		name: profile_even_odd,
		file: "gs_even_odd.snek",
		input: "1397541",
		expected: "false",
	},
	{
		name: profile_loopsum,
		file: "loopsum.snek",
		input: "2000000000",
		expected: "2000000000",
	},
	{
		name: profile_complex_expr,
		file: "gs_complex_expr.snek",
		expected: "1000000001",
	},
	{
		name: profile_simple_calc,
		file: "gs_simple_calc.snek",
		input: "10",
		expected: "1000000001",
	},
}
