package argparse;

public class Argument {

    public <R> R type(StringFunction<R> fun) {
        return fun.apply("");
    }
}