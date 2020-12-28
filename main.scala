import io.StdIn.readLine
import annotation.tailrec

abstract class Distribution {
    def get_critical_region(): Unit
    def pretty_calc_p(): Double
}

class Binomial(test_stat: Int, num_trials: Int, trial_prob: Double, sig_level: Double, tail: String) extends Distribution {
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
    private def less_than_probability(): Double ={
        return (0 to test_stat).map(probability(_: Int)).sum.toDouble
    }
    //calculates greater than or equal to
    private def greater_than_probability(): Double ={
        return (test_stat to num_trials).map(probability(_: Int)).sum.toDouble
    }

    @tailrec
    final def calculate_critical_region(lower: Int = 0, upper: Int = num_trials): Int ={
        val middle: Int = lower + (upper - lower)/2
        //if we have whitled it down to only 2 possible valus
        if (upper - lower <= 1){
            if (((new Binomial(lower, num_trials, trial_prob, sig_level, tail)).calc_p()) < sig_level) return lower else return upper
        }
        //if currently inside acceptance region for H0
        if (((new Binomial(middle, num_trials, trial_prob, sig_level, tail)).calc_p()) > sig_level){
            if (tail == "less") return calculate_critical_region(lower, middle) else return calculate_critical_region(middle, upper)
        }
        //if currently inside rejection region for H0
        else{
            if (tail == "greater") return calculate_critical_region(lower, middle) else return calculate_critical_region(middle, upper)
        }
    }
    //unit means we don't return anything
    override def get_critical_region(): Unit ={
        if (tail == "greater"){
            val critical_value: Int = calculate_critical_region()
            println(s"Critical region is X≥$critical_value")
        }
        else if (tail == "less"){
            val critical_value: Int = calculate_critical_region()
            println(s"Critical region is X≤$critical_value")
        }

    }
    override def pretty_calc_p(): Double = {
        if (tail == "greater") println("Calculating P(X≥a)") else if (tail == "less") println("Calculating P(X≤a)")
        return calc_p()
    }
    private def calc_p(): Double ={
        if (tail == "greater") {
            return greater_than_probability()
        }
        else {
            return less_than_probability()
        }
    }
}

class TwoTailedBinomial(test_stat: Int, num_trials: Int, trial_prob: Double, sig_level: Double) extends Distribution {
    println("Conducting a two-tailed test")
    println("Halving significance level")
    def select_tail(): String ={
        val expected_mean: Double = num_trials * trial_prob
        println(s"Expected mean value of sample $expected_mean")
        if (test_stat > expected_mean) {
            println("Number of successes is greater than mean value")
            return "greater"
        }
        else {
            println("Number of successes is less than mean value")
            return "lower"
        }
    }
    val selected_tail: String = select_tail()
    
    override def get_critical_region(): Unit ={
        val lower_critical_value: Int = new Binomial(test_stat, num_trials, trial_prob, sig_level, "less").calculate_critical_region()
        val upper_critical_value: Int = new Binomial(test_stat, num_trials, trial_prob, sig_level, "greater").calculate_critical_region()
        println(s"Critical region is $lower_critical_value≥X or $upper_critical_value≤X")

    }
    override def pretty_calc_p(): Double = {
        return new Binomial(test_stat, num_trials, trial_prob, sig_level, selected_tail).pretty_calc_p()
    }

}   

object HypothesisTesting extends App {
    def adjust_sig(sig: Double, tail: String): Double ={if (tail != "two tail") sig else sig/2}

    def ToDouble(s: String): Double ={
        try{
            return s.toDouble
        }
        catch {
            case e: NumberFormatException => return s.split("/")(0).toDouble/s.split("/")(1).toDouble
        }
    }
    val test_type: String = (readLine("Under the alternative hypothesis has the probability of success: 1) Increased 2) Decreased 3) Don't know  ")) match {case "1" => "greater" case "2" => "less" case "3" => "two tail"}
    val sig_level: Double = adjust_sig(ToDouble(readLine("Enter the significance level for the test ")), test_type)
    val trial_prob: Double = ToDouble(readLine("Under the null hypothesis what is the probability of success for a single trial "))
    val sample_size: Int = readLine("What is the size of the sample  ").toInt
    val sample_successes: Int = readLine("How many successes are there in the sample  ").toInt
    println("")

    println("")
    def get_dist(test_stat: Int, num_trials: Int, trial_prob: Double, sig_level: Double, tail: String): Distribution ={
        if (tail != "two tail") {
            return new Binomial(test_stat, num_trials, trial_prob, sig_level, tail)
        }
        else {
            return new TwoTailedBinomial(test_stat, num_trials, trial_prob, sig_level)
        }
    } 
    val b: Distribution = get_dist(sample_successes, sample_size, trial_prob, sig_level, test_type)

    val p: Double = b.pretty_calc_p()
    if (p > sig_level) {
    	println(s"p=$p, insufficient evidence to reject null hypothesis")
    }
    else {
    	println(s"p=$p, sufficient evidence to reject null hypothesis")
    }
    b.get_critical_region()
}
