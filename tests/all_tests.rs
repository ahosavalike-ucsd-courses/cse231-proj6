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
		file: "even_odd.snek",
		input: "1397541",
		expected: "1397541\nfalse\nfalse",
	},
	{
		name: profile_loopsum,
		file: "loopsum.snek",
		input: "2000000000",
		expected: "2000000000",
	},
}
