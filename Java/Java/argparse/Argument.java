package argparse;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Parsed argument
 */
public class Argument {
    List<String> data = new ArrayList<String>();

    /**
     *
     * @param <R> StringFunction return type
     * @param fun StringFunction to apply
     * @return fun applied to contents
     */
    public <R> List<R> type(StringFunction<? extends R> fun) {
        return data.stream().map(fun).collect(Collectors.toList());
    }

    /**
     *
     * @return argument data
     */
    public List<String> get() {
        return data;
    }

    /**
     * Flag test shorthand
     *
     * @return if argument is truthy
     */
    public boolean isTrue() {
        return !data.isEmpty() && Boolean.parseBoolean(data.get(0));
    }

    /**
     * complement to isTrue
     *
     * @return if argument is falsy
     */
    public boolean isFalse() {
        return !isTrue();
    }
}