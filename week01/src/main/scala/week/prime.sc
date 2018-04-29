def isPrime(n: Int): Boolean = 2 until n forall ( n % _ != 0)

isPrime(1)

isPrime(2)

isPrime(3)

isPrime(4)

isPrime(13)
