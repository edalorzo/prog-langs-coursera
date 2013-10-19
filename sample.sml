
val x = 10 before print("Hello\n")

val a = 10
val b = 10

val r = case Int.compare(a,b) of
			LESS => b - a |
			GREATER => a -b |
			EQUAL => 0


datatype 'a list = Empty | Cons of 'a * 'a list


