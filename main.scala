import math.pow
import io.StdIn.readLine

object HypothesisTesting extends App {
	//bigint is required beacuse factorial is very big for moderately large n, who knew?
	def factorial(n: Int): BigInt ={
		return if (n > 1) n*factorial(n-1) else 1
	}
	//returns BigDecimal beacuse it's going to be multiplied with more decimals
	def num_combinations(n: Int, r: Int): BigDecimal ={
		val denominator: BigInt = factorial(r) * factorial(n-r)
		return BigDecimal(factorial(n)/denominator)
	}

	def binomial_probability(x: Int, num_trials: Int, p: Double): Double ={
		return (num_combinations(num_trials, x) * BigDecimal(pow(p, x)) * BigDecimal(pow(1-p, num_trials-x))).toDouble
	}
	//calculates less than or equal to
	def less_than_binomial_probability(x: Int, num_trials: Int, p: Double): Double ={
		(0 to x).map(binomial_probability(_: Int, num_trials, p)).sum
	}
	//calculates greater than or equal to
	def greater_than_binomial_probability(x: Int, num_trials: Int, p: Double): Double ={
		(x to num_trials).map(binomial_probability(_: Int, num_trials, p)).sum
	}

	//TODO: Add more distributions
	val distribution: String = "binomial"
	if (distribution == "binomial") {
		val test_type: String = (readLine("Under the alternative hypothesis has the probability of success: 1) Increased 2) Decreased 3) Don't know  ")) match {case "1" => "greater" case "2" => "less" case "3" => "two tail"}
		var sig_level: Double = readLine("Enter the significance level for the test ").toDouble
		val trial_prob: Double = readLine("Under the null hypothesis what is the probability of success for a single trial ").toDouble
		val sample_size: Int = readLine("What is the size of the sample  ").toInt
		val sample_successes: Int = readLine("How many successes are there in the sample  ").toInt
		var p: Double = 0
		println("")

		println("")
		if (test_type == "greater") {
			println("Calculating P(X≤a)")
			p = greater_than_binomial_probability(sample_successes, sample_size, trial_prob)
		}
		else if (test_type == "less") {
			println("Calculating P(X≥a)")
			p = less_than_binomial_probability(sample_successes, sample_size, trial_prob)
		}

		else {
			println("Conducting a two-tailed test")
			sig_level /= 2
			println("Halving significance level")

			val expected_mean: Double = sample_size * trial_prob
			println(s"Expected mean value of sample $expected_mean")
			if (sample_successes > expected_mean) {
				println("Number of successes is greater than mean value, calculating P(X≥a)")
				p = greater_than_binomial_probability(sample_successes, sample_size, trial_prob)
			}
			else {
				println("Number of successes is less than mean value, calculating P(X≤a)")
				p = less_than_binomial_probability(sample_successes, sample_size, trial_prob)
			}

		}
		if (p > sig_level) {
			println(s"p=$p, insufficient evidence to reject null hypothesis")
		}
		else {
			println(s"p=$p, sufficient evidence to reject null hypothesis")
		}
	}	
}