package argparse;

import java.util.HashMap;

/**
 * HashMap wrapper, providing additional methods for options
 */
public class ArgResult extends HashMap<String, Argument> {
    /**
     * Equivalent to {@link HashMap#containsKey containsKey}
     *
     * @param opt key
     * @return whether map contains key
     */
    public boolean hasOpt(String opt) {
        return this.containsKey(opt);
    }

    /**
     * Equivalent to {@link HashMap#containsKey containsKey} , keyed by destination
     *
     * @param opt key
     * @return whether map contains key
     */
    public boolean hasOpt(Option opt) {
        return this.containsKey(opt.destination);
    }

    /**
     * Get, but keyed by Option destination
     *
     * @param opt option
     * @return corresponding Argument
     */
    Argument get(Option opt) {
        return this.get(opt.destination);
    }
}