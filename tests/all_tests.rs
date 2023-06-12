mod infra;

success_tests! {
	{
		name: ff_tree_cycle_28,
		file: "ff_tree_cycle.snek",
		expected: "[[1, [[2, [...]]], [[3, [...]]]]]",
	},
	{
		name: ff_tree_cycle_29,
		file: "ff_tree_cycle.snek",
		input: "true",
		expected: "[[1, [[2, [...]]], [[3, [...]]]]]",
	},
	{
		name: ff_tree_cycle_30,
		file: "ff_tree_cycle.snek",
		input: "true",
		heap_size: 25,
		expected: "[[1, [[2, [...]]], [[3, [...]]]]]",
	},
	{
		name: ff_tree_cycle_32,
		file: "ff_tree_cycle.snek",
		input: "false",
		heap_size: 25,
		expected: "[[1, [[2, [...]]], [[3, [...]]]]]",
	},
	{
		name: nested_arith1_1,
		file: "nested_arith1.snek",
		expected: "25",
	},
	{
		name: student_merge_sort_122,
		file: "student_merge_sort.snek",
		input: "1000",
		expected: "[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175, 176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 191, 192, 193, 194, 195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207, 208, 209, 210, 211, 212, 213, 214, 215, 216, 217, 218, 219, 220, 221, 222, 223, 224, 225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 239, 240, 241, 242, 243, 244, 245, 246, 247, 248, 249, 250, 251, 252, 253, 254, 255, 256, 257, 258, 259, 260, 261, 262, 263, 264, 265, 266, 267, 268, 269, 270, 271, 272, 273, 274, 275, 276, 277, 278, 279, 280, 281, 282, 283, 284, 285, 286, 287, 288, 289, 290, 291, 292, 293, 294, 295, 296, 297, 298, 299, 300, 301, 302, 303, 304, 305, 306, 307, 308, 309, 310, 311, 312, 313, 314, 315, 316, 317, 318, 319, 320, 321, 322, 323, 324, 325, 326, 327, 328, 329, 330, 331, 332, 333, 334, 335, 336, 337, 338, 339, 340, 341, 342, 343, 344, 345, 346, 347, 348, 349, 350, 351, 352, 353, 354, 355, 356, 357, 358, 359, 360, 361, 362, 363, 364, 365, 366, 367, 368, 369, 370, 371, 372, 373, 374, 375, 376, 377, 378, 379, 380, 381, 382, 383, 384, 385, 386, 387, 388, 389, 390, 391, 392, 393, 394, 395, 396, 397, 398, 399, 400, 401, 402, 403, 404, 405, 406, 407, 408, 409, 410, 411, 412, 413, 414, 415, 416, 417, 418, 419, 420, 421, 422, 423, 424, 425, 426, 427, 428, 429, 430, 431, 432, 433, 434, 435, 436, 437, 438, 439, 440, 441, 442, 443, 444, 445, 446, 447, 448, 449, 450, 451, 452, 453, 454, 455, 456, 457, 458, 459, 460, 461, 462, 463, 464, 465, 466, 467, 468, 469, 470, 471, 472, 473, 474, 475, 476, 477, 478, 479, 480, 481, 482, 483, 484, 485, 486, 487, 488, 489, 490, 491, 492, 493, 494, 495, 496, 497, 498, 499, 500, 501, 502, 503, 504, 505, 506, 507, 508, 509, 510, 511, 512, 513, 514, 515, 516, 517, 518, 519, 520, 521, 522, 523, 524, 525, 526, 527, 528, 529, 530, 531, 532, 533, 534, 535, 536, 537, 538, 539, 540, 541, 542, 543, 544, 545, 546, 547, 548, 549, 550, 551, 552, 553, 554, 555, 556, 557, 558, 559, 560, 561, 562, 563, 564, 565, 566, 567, 568, 569, 570, 571, 572, 573, 574, 575, 576, 577, 578, 579, 580, 581, 582, 583, 584, 585, 586, 587, 588, 589, 590, 591, 592, 593, 594, 595, 596, 597, 598, 599, 600, 601, 602, 603, 604, 605, 606, 607, 608, 609, 610, 611, 612, 613, 614, 615, 616, 617, 618, 619, 620, 621, 622, 623, 624, 625, 626, 627, 628, 629, 630, 631, 632, 633, 634, 635, 636, 637, 638, 639, 640, 641, 642, 643, 644, 645, 646, 647, 648, 649, 650, 651, 652, 653, 654, 655, 656, 657, 658, 659, 660, 661, 662, 663, 664, 665, 666, 667, 668, 669, 670, 671, 672, 673, 674, 675, 676, 677, 678, 679, 680, 681, 682, 683, 684, 685, 686, 687, 688, 689, 690, 691, 692, 693, 694, 695, 696, 697, 698, 699, 700, 701, 702, 703, 704, 705, 706, 707, 708, 709, 710, 711, 712, 713, 714, 715, 716, 717, 718, 719, 720, 721, 722, 723, 724, 725, 726, 727, 728, 729, 730, 731, 732, 733, 734, 735, 736, 737, 738, 739, 740, 741, 742, 743, 744, 745, 746, 747, 748, 749, 750, 751, 752, 753, 754, 755, 756, 757, 758, 759, 760, 761, 762, 763, 764, 765, 766, 767, 768, 769, 770, 771, 772, 773, 774, 775, 776, 777, 778, 779, 780, 781, 782, 783, 784, 785, 786, 787, 788, 789, 790, 791, 792, 793, 794, 795, 796, 797, 798, 799, 800, 801, 802, 803, 804, 805, 806, 807, 808, 809, 810, 811, 812, 813, 814, 815, 816, 817, 818, 819, 820, 821, 822, 823, 824, 825, 826, 827, 828, 829, 830, 831, 832, 833, 834, 835, 836, 837, 838, 839, 840, 841, 842, 843, 844, 845, 846, 847, 848, 849, 850, 851, 852, 853, 854, 855, 856, 857, 858, 859, 860, 861, 862, 863, 864, 865, 866, 867, 868, 869, 870, 871, 872, 873, 874, 875, 876, 877, 878, 879, 880, 881, 882, 883, 884, 885, 886, 887, 888, 889, 890, 891, 892, 893, 894, 895, 896, 897, 898, 899, 900, 901, 902, 903, 904, 905, 906, 907, 908, 909, 910, 911, 912, 913, 914, 915, 916, 917, 918, 919, 920, 921, 922, 923, 924, 925, 926, 927, 928, 929, 930, 931, 932, 933, 934, 935, 936, 937, 938, 939, 940, 941, 942, 943, 944, 945, 946, 947, 948, 949, 950, 951, 952, 953, 954, 955, 956, 957, 958, 959, 960, 961, 962, 963, 964, 965, 966, 967, 968, 969, 970, 971, 972, 973, 974, 975, 976, 977, 978, 979, 980, 981, 982, 983, 984, 985, 986, 987, 988, 989, 990, 991, 992, 993, 994, 995, 996, 997, 998, 999, 1000]",
	},
	{
		name: binding_nested_1,
		file: "binding_nested.snek",
		expected: "1",
	},
	{
		name: func_nested_mutual_recursive_tail_3,
		file: "func_nested_mutual_recursive_tail.snek",
		input: "1",
		expected: "243",
	},
	{
		name: func_nested_mutual_recursive_tail_4,
		file: "func_nested_mutual_recursive_tail.snek",
		input: "10",
		expected: "270",
	},
	{
		name: func_nested_mutual_recursive_tail_5,
		file: "func_nested_mutual_recursive_tail.snek",
		input: "100",
		expected: "300",
	},
	{
		name: func_nested_mutual_recursive_tail_6,
		file: "func_nested_mutual_recursive_tail.snek",
		input: "200",
		expected: "200",
	},
	{
		name: shadowed_binding_succ1_1,
		file: "shadowed_binding_succ1.snek",
		expected: "7",
	},
	{
		name: binding_long_1,
		file: "binding_long.snek",
		expected: "1073741824",
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
		name: ff_nested_alloc_7,
		file: "ff_nested_alloc.snek",
		input: "10",
		expected: "0",
	},
	{
		name: ff_nested_alloc_9,
		file: "ff_nested_alloc.snek",
		input: "10",
		heap_size: 10,
		expected: "0",
	},
	{
		name: ff_nested_alloc_10,
		file: "ff_nested_alloc.snek",
		input: "100",
		heap_size: 10,
		expected: "0",
	},
	{
		name: ff_nested_alloc_11,
		file: "ff_nested_alloc.snek",
		input: "100",
		heap_size: 100,
		expected: "0",
	},
	{
		name: type_check_succ5_1,
		file: "type_check_succ5.snek",
		expected: "true",
	},
	{
		name: ed482_1_83,
		file: "ed482_1.snek",
		expected: "nil\nfalse\n[-1, [-100, nil, nil], [1, nil, nil]]\ntrue\ntrue\ntrue\nfalse\n[-1, [-100, nil, nil], [1, nil, [100, nil, nil]]]\ntrue\nfalse\n[-1, [-100, nil, nil], [1, [0, nil, nil], [100, nil, nil]]]\ntrue\nfalse\n[1, nil, [2, nil, [3, nil, nil]]]\n[1, nil, [2, nil, [3, nil, [4, nil, [5, nil, nil]]]]]\n0",
	},
	{
		name: ff_multiple_empty_gc_9,
		file: "ff_multiple_empty_gc.snek",
		input: "10",
		expected: "11",
	},
	{
		name: ff_multiple_empty_gc_11,
		file: "ff_multiple_empty_gc.snek",
		input: "10",
		heap_size: 10,
		expected: "11",
	},
	{
		name: ff_multiple_empty_gc_12,
		file: "ff_multiple_empty_gc.snek",
		input: "100",
		heap_size: 10,
		expected: "101",
	},
	{
		name: ff_multiple_empty_gc_13,
		file: "ff_multiple_empty_gc.snek",
		input: "100",
		heap_size: 100,
		expected: "101",
	},
	{
		name: student_block_mutable_typecheck_7,
		file: "student_block_mutable_typecheck.snek",
		expected: "3\n3",
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
		name: student_gc_odd2_42,
		file: "student_gc_odd2.snek",
		input: "128",
		expected: "89",
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
		name: func_f1_2,
		file: "func_f1.snek",
		expected: "1\n1",
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
		name: ff_fragment_heap_20,
		file: "ff_fragment_heap.snek",
		expected: "[[0, 2, 4, 6, 8, 10]]",
	},
	{
		name: ff_fragment_heap_22,
		file: "ff_fragment_heap.snek",
		input: "11",
		heap_size: 18,
		expected: "[[0, 2, 4, 6, 8, 10]]",
	},
	{
		name: ff_fragment_heap_24,
		file: "ff_fragment_heap.snek",
		input: "21",
		heap_size: 28,
		expected: "[[0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20]]",
	},
	{
		name: ff_fragment_heap_26,
		file: "ff_fragment_heap.snek",
		input: "101",
		heap_size: 108,
		expected: "[[0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 42, 44, 46, 48, 50, 52, 54, 56, 58, 60, 62, 64, 66, 68, 70, 72, 74, 76, 78, 80, 82, 84, 86, 88, 90, 92, 94, 96, 98, 100]]",
	},
	{
		name: shadowed_binding_succ5_1,
		file: "shadowed_binding_succ5.snek",
		expected: "5",
	},
	{
		name: student_cleanup_nested_35,
		file: "student_cleanup_nested.snek",
		input: "1000",
		heap_size: 4008,
		expected: "1000",
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
		name: ff_cycle_11,
		file: "ff_cycle.snek",
		expected: "[1, 2, [...]]\n[[...], 2, 3]",
	},
	{
		name: ff_cycle_12,
		file: "ff_cycle.snek",
		heap_size: 5,
		expected: "[1, 2, [...]]\n[[...], 2, 3]",
	},
	{
		name: ed482_4_7,
		file: "ed482_4.snek",
		input: "1",
		expected: "0",
	},
	{
		name: ff_dag_30,
		file: "ff_dag.snek",
		expected: "[[1, [[2, [[4, [[5]]]]]], [[3, [[4, [[5]]]], [[5]]]], [[4, [[5]]]], [[5]]]]",
	},
	{
		name: func_f2_2,
		file: "func_f2.snek",
		expected: "1\n1",
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
		expected: "10\n9\n8\n7\n6\n5\n4\n3\n2\n1\n1\n2\n6\n24\n120\n720\n5040\n40320\n362880\n3628800",
	},
	{
		name: student_block_mutable_typedivergence_7,
		file: "student_block_mutable_typedivergence.snek",
		expected: "4",
	},
	{
		name: shadowed_binding_succ4_1,
		file: "shadowed_binding_succ4.snek",
		expected: "18",
	},
	{
		name: cbn_104,
		file: "cbn.snek",
		expected: "[1, 0]",
	},
	{
		name: binding_chain_1,
		file: "binding_chain.snek",
		expected: "3",
	},
	{
		name: func_fact_tail_3,
		file: "func_fact_tail.snek",
		input: "10",
		expected: "3628800",
	},
	{
		name: func_fact_tail_4,
		file: "func_fact_tail.snek",
		input: "20",
		expected: "2432902008176640000",
	},
	{
		name: student_insertion_sort_60,
		file: "student_insertion_sort.snek",
		input: "1000",
		heap_size: 1002,
		expected: "true",
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
		name: ed482_2_8,
		file: "ed482_2.snek",
		expected: "10\n10\n0",
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
		name: student_gc_odd_41,
		file: "student_gc_odd.snek",
		input: "10000",
		heap_size: 40002,
		expected: "10001",
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
		name: ff_weird_gc_7,
		file: "ff_weird_gc.snek",
		expected: "0\n[0, 2, 0]",
	},
	{
		name: ff_weird_gc_9,
		file: "ff_weird_gc.snek",
		input: "false",
		heap_size: 5,
		expected: "0\n[0, 2, 0]",
	},
	{
		name: shadowed_binding_succ3_1,
		file: "shadowed_binding_succ3.snek",
		expected: "5",
	},
	{
		name: ff_stack_multiref_endref_17,
		file: "ff_stack_multiref_endref.snek",
		expected: "[1, 2, 4]\n[1, 2, 3]\n[0, 0, 0, 0, 0]",
	},
	{
		name: ff_stack_multiref_endref_18,
		file: "ff_stack_multiref_endref.snek",
		input: "false",
		heap_size: 10,
		expected: "[1, 2, 4]\n[1, 2, 3]\n[0, 0, 0, 0, 0]",
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
	{
		name: ed482_3_9,
		file: "ed482_3.snek",
		expected: "61",
	},
}

runtime_error_tests! {
	{
		name: ff_tree_cycle_31,
		file: "ff_tree_cycle.snek",
		input: "true",
		heap_size: 24,
		expected: "out of memory",
	},
	{
		name: ff_tree_cycle_33,
		file: "ff_tree_cycle.snek",
		input: "false",
		heap_size: 24,
		expected: "out of memory",
	},
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
		name: ff_nested_alloc_8,
		file: "ff_nested_alloc.snek",
		input: "10",
		heap_size: 9,
		expected: "out of memory",
	},
	{
		name: ff_multiple_empty_gc_10,
		file: "ff_multiple_empty_gc.snek",
		input: "10",
		heap_size: 9,
		expected: "out of memory",
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
		name: ff_fragment_heap_21,
		file: "ff_fragment_heap.snek",
		input: "11",
		heap_size: 17,
		expected: "out of memory",
	},
	{
		name: ff_fragment_heap_23,
		file: "ff_fragment_heap.snek",
		input: "21",
		heap_size: 27,
		expected: "out of memory",
	},
	{
		name: ff_fragment_heap_25,
		file: "ff_fragment_heap.snek",
		input: "101",
		heap_size: 107,
		expected: "out of memory",
	},
	{
		name: student_cleanup_nested_36,
		file: "student_cleanup_nested.snek",
		input: "1000",
		heap_size: 4007,
		expected: "out of memory",
	},
	{
		name: ff_cycle_13,
		file: "ff_cycle.snek",
		input: "false",
		heap_size: 4,
		expected: "out of memory",
	},
	{
		name: if_expr_input_2,
		file: "if_expr_input.snek",
		input: "665",
		expected: "invalid argument",
	},
	{
		name: student_insertion_sort_61,
		file: "student_insertion_sort.snek",
		input: "1000",
		heap_size: 1001,
		expected: "out of memory",
	},
	{
		name: invalid_argument_fail6_1,
		file: "invalid_argument_fail6.snek",
		expected: "invalid argument",
	},
	{
		name: student_gc_odd_42,
		file: "student_gc_odd.snek",
		input: "10000",
		heap_size: 40001,
		expected: "out of memory",
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
		name: ff_weird_gc_8,
		file: "ff_weird_gc.snek",
		input: "false",
		heap_size: 4,
		expected: "out of memory",
	},
	{
		name: ff_stack_multiref_endref_19,
		file: "ff_stack_multiref_endref.snek",
		input: "false",
		heap_size: 9,
		expected: "out of memory",
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
		name: unbound_identifier_fail1_1,
		file: "unbound_identifier_fail1.snek",
		expected: "Unbound variable identifier y",
	},
}

