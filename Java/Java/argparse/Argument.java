package argparse;

/**
 * a parsed argument
 */
public class Argument {

    /**
     *
     * @param <R> return type
     * @param fun StringFunction to apply
     * @return fun applied to contents
     */
    public <R> R type(StringFunction<R> fun) {
        return fun.apply("");
    }
}