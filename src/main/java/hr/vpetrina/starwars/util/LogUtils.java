package hr.vpetrina.starwars.util;

import java.util.logging.Level;
import java.util.logging.Logger;

public class LogUtils {

    private LogUtils() {}

    private static final Logger logger = Logger.getLogger(LogUtils.class.getName());

    public static void logInfo(String message) {
        logger.log(Level.INFO, message);
    }

    public static void logWarning(String message) {
        logger.log(Level.WARNING, message);
    }

    public static void logSevere(String message) {
        logger.log(Level.SEVERE, message);
    }
}