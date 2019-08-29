package argparse;

/**
 * Argument arity modifier.
 */
public enum ArityMod {
    /**
     * Take exactly n
     */
    FIXED,
    /**
     * Take until next option
     */
    GREEDY,
    /**
     * GREEDY with minimum n
     */
    LOWER_BOUND,
    /**
     * Consume remaining arguments. NOTE: a given parser will only acknowledge the
     * first REST; evaluated after all other options
     */
    REMAINDER,
    /**
     * Take up-to n
     */
    UPPER_BOUND
}