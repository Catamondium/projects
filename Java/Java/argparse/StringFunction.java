package argparse;

import java.util.function.Function;

/**
 * Unary String -> R function
 * 
 * @param <R> Return type
 */
public interface StringFunction<R> extends Function<String, R> {

}