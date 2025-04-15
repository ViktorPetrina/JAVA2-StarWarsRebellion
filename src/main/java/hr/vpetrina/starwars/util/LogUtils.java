package hr.vpetrina.starwars.util;

import static java.lang.System.out;

public class LogUtils {

    private LogUtils() {}

    private static final String RESET = "\u001B[0m";
    private static final String WHITE = "\u001B[37m";
    private static final String RED = "\u001B[31m";

    public static void logInfo(String message) {
        out.println(WHITE + message + RESET);
    }

    public static void logWarning(String message) {
        out.println(message);
    }

    public static void logSevere(String message) {
        out.println(RED + message + RESET);
    }
}
