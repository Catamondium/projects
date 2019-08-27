package argparse;

import java.util.*;

/**
 * Parser builder
 */
public class Parser {
    Optional<String> desc = Optional.empty();
    List<Option> positionals = new ArrayList<Option>();
    Map<String, Option> keywords = new HashMap<String, Option>();

    /**
     * Create Parser w/ description
     *
     * @param desc description
     */
    public Parser(String desc) {
        this.desc = Optional.of(desc);
    }

    /**
     * Create Parser w/o description
     */
    public Parser() {

    }

    /**
     * Add a sequence of options to Parser
     *
     * @param options options
     * @return this
     */
    public Parser addOptions(Option... options) {
        for (Option o : options) {
            if (o.isKeyword()) {
                keywords.put(o.destination, o);
            } else {
                positionals.add(o);
            }
        }
        return this;
    }

    /**
     * Parses with given options
     *
     * @param argv command line arguments
     * @return mapping of Option.destination to Argument
     */
    public Map<String, Argument> parse(String[] argv) {
        return new HashMap<String, Argument>();
    }
}
