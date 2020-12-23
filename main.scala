import io.StdIn.readLine
import annotation.tailrec
import scala.math.Ordering.Double.TotalOrdering

class Binomial(test_stat: Int, num_trials: Int, trial_prob: Double, sig_level: Double, tail: String) {
    @tailrec
    //bigint is required beacuse factorial is very big for moderately large n, who knew?
    private def limited_factorial(n: BigInt, lim: BigInt, result: BigInt = 1): BigInt ={
        if (n <= lim) result else limited_factorial(n-1, lim, result*n)
    }
    private def factorial(n: Int): BigInt ={
        return limited_factorial(n, 1)
    }
    //returns BigDecimal because it's going to be multiplied with more decimals
    private def num_combinations(n: Int, r: Int): BigDecimal ={
        return BigDecimal(limited_factorial(n, r)/factorial(n-r))
    }

    private def probability(x: Int): BigDecimal ={
        val p: BigDecimal = BigDecimal(trial_prob)
        return num_combinations(num_trials, x) * p.pow(x) * (1-p).pow(num_trials-x)
    }
    //calculates less than or equal to
    def less_than_probability(): Double ={
        return (0 to test_stat).map(probability(_: Int)).sum.toDouble
    }
    //calculates greater than or equal to
    def greater_than_probability(): Double ={
        return (test_stat to num_trials).map(probability(_: Int)).sum.toDouble
    }

    @tailrec
    private def calculate_critical_region(lower: Int = 0, upper: Int = num_trials): Int ={
        val middle: Int = lower + (upper - lower)/2
        Thread.sleep(500)
        println((new Binomial(middle, num_trials, trial_prob, sig_level, tail)).calc_p())
        if (upper - lower <= 1){
            if (((new Binomial(lower, num_trials, trial_prob, sig_level, tail)).calc_p()) < sig_level) return lower else return upper
        }
        if (((new Binomial(middle, num_trials, trial_prob, sig_level, tail)).calc_p()) > sig_level){
            if (tail == "less") return calculate_critical_region(lower, middle) else return calculate_critical_region(middle, upper)
        }
        else{
            if (tail == "greater") return calculate_critical_region(lower, middle) else return calculate_critical_region(middle, upper)
        }
    }
    //unit means we don't return anything
    def get_critical_region(): Unit ={
        if (tail == "greater"){
            val critical_value: Int = calculate_critical_region()
            println(s"Critical region is X≥$critical_value")
        }
        else if (tail == "less"){
            val critical_value: Int = calculate_critical_region()
            println(s"Critical region is X≤$critical_value")
        }
        else {

        }
    }
    def pretty_calc_p(): Double = {
        if (tail == "greater") println("Calculating P(X≥a)") else if (tail == "less") println("Calculating P(X≤a)")
        return calc_p()
    }
    private def calc_p(): Double ={
        if (tail == "greater") {
            return greater_than_probability()
        }
        else if (tail == "less") {
            return less_than_probability()
        }

        else {
            println("Conducting a two-tailed test")
            println("Halving significance level")

            val expected_mean: Double = num_trials * trial_prob
            println(s"Expected mean value of sample $expected_mean")
            if (test_stat > expected_mean) {
                println("Number of successes is greater than mean value")
                val new_b: Binomial = new Binomial(test_stat, num_trials, trial_prob, sig_level, "greater")
                new_b.get_critical_region()
                return new_b.pretty_calc_p()
            }
            else {
                println("Number of successes is less than mean value")
                (new Binomial(test_stat, num_trials, trial_prob, sig_level, "less")).pretty_calc_p()
            }
        }
    }
}

object HypothesisTesting extends App {
	def adjust_sig(sig: Double, tail: String): Double ={if (tail != "two tail") sig else sig/2}

	val test_type: String = (readLine("Under the alternative hypothesis has the probability of success: 1) Increased 2) Decreased 3) Don't know  ")) match {case "1" => "greater" case "2" => "less" case "3" => "two tail"}
	val sig_level: Double = adjust_sig(readLine("Enter the significance level for the test ").toDouble, test_type)
	val trial_prob: Double = readLine("Under the null hypothesis what is the probability of success for a single trial ").toDouble
	val sample_size: Int = readLine("What is the size of the sample  ").toInt
	val sample_successes: Int = readLine("How many successes are there in the sample  ").toInt
	println("")

	println("")
    val b: Binomial = new Binomial(sample_successes, sample_size, trial_prob, sig_level, test_type)
	val p: Double = b.pretty_calc_p()
	if (p > sig_level) {
		println(s"p=$p, insufficient evidence to reject null hypothesis")
	}
	else {
		println(s"p=$p, sufficient evidence to reject null hypothesis")
	}
    b.get_critical_region()
}
