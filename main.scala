import math.pow
import io.StdIn.readLine
import annotation.tailrec

object BinomalTest extends App {
	@tailrec
	//bigint is required beacuse factorial is very big for moderately large n, who knew?
	def limited_factorial(n: BigInt, lim: BigInt, result: BigInt = 1): BigInt ={
		if (n <= lim) result else limited_factorial(n-1, lim, result*n)
	}
	def factorial(n: Int): BigInt ={
		return limited_factorial(n, 1)
	}
	//returns BigDecimal because it's going to be multiplied with more decimals
	def num_combinations(n: Int, r: Int): BigDecimal ={
		return BigDecimal(limited_factorial(n, r)/factorial(n-r))
	}

	def binomial_probability(x: Int, num_trials: Int, p: Double): BigDecimal ={
		var bigd_p: BigDecimal = BigDecimal(p)
		return num_combinations(num_trials, x) * bigd_p.pow(x) * (1-bigd_p).pow(num_trials-x)
	}
	//calculates less than or equal to
	def less_than_binomial_probability(x: Int, num_trials: Int, p: Double): Double ={
		((0 to x).map(binomial_probability(_: Int, num_trials, p)).sum).toDouble
	}
	//calculates greater than or equal to
	def greater_than_binomial_probability(x: Int, num_trials: Int, p: Double): Double ={
		((x to num_trials).map(binomial_probability(_: Int, num_trials, p)).sum).toDouble
	}

	def calculate_p(tail: String, sample_successes: Int, sample_size: Int, trial_prob: Double): Double ={
		if (tail == "greater") {
			println("Calculating P(X≥a)")
			return greater_than_binomial_probability(sample_successes, sample_size, trial_prob)
		}
		else if (tail == "less") {
			println("Calculating P(X≤a)")
			return less_than_binomial_probability(sample_successes, sample_size, trial_prob)
		}

		else {
			println("Conducting a two-tailed test")
			println("Halving significance level")

			val expected_mean: Double = sample_size * trial_prob
			println(s"Expected mean value of sample $expected_mean")
			if (sample_successes > expected_mean) {
				println("Number of successes is greater than mean value, calculating P(X≥a)")
				return calculate_p("greater", sample_successes, sample_size, trial_prob)
			}
			else {
				println("Number of successes is less than mean value, calculating P(X≤a)")
				return calculate_p("less", sample_successes, sample_size, trial_prob)
			}
		}
	}

	def adjust_sig(sig: Double, tail: String): Double ={if (tail != "two tail") sig else sig/2}

	val test_type: String = (readLine("Under the alternative hypothesis has the probability of success: 1) Increased 2) Decreased 3) Don't know  ")) match {case "1" => "greater" case "2" => "less" case "3" => "two tail"}
	val sig_level: Double = adjust_sig(readLine("Enter the significance level for the test ").toDouble, test_type)
	val trial_prob: Double = readLine("Under the null hypothesis what is the probability of success for a single trial ").toDouble
	val sample_size: Int = readLine("What is the size of the sample  ").toInt
	val sample_successes: Int = readLine("How many successes are there in the sample  ").toInt
	println("")

	println("")
	val p: Double = calculate_p(test_type, sample_successes, sample_size, trial_prob)
	if (p > sig_level) {
		println(s"p=$p, insufficient evidence to reject null hypothesis")
	}
	else {
		println(s"p=$p, sufficient evidence to reject null hypothesis")
	}
}
