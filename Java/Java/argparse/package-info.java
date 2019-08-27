/**
 * Command line argument parser. Based on Python's argparse library.
 *
 * Operates on 2 builders:
 *
 * {@link argparse.Option Option}, to build parsing targets.
 *
 * {@link argparse.Parser Parser}, to provide the Options to.
 *
 * {@link argparse.Parser#parse Parser.parse} will return a mapping of Strings
 * to {@link argparse.Argument Arguments}
 *
 * {@link argparse.Argument Argument} is equipped with methods to return
 * transformed contents directly Unfortunately this is post-parse, so uncaught
 * exceptions will display.
 *
 * @version %I% %G%
 */

package argparse;