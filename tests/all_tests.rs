mod infra;

runtime_error_tests! {
	{
		name: print1_1,
		file: "print1.snek",
		expected: "invalid argument",
	},
}

success_tests! {
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
}

static_error_tests! {
	{
		name: duplicate_params_5,
		file: "duplicate_params.snek",
		expected: "",
	},
}

