package hr.vpetrina.starwars.exception;

public class ConcurrentAccessException extends RuntimeException {

    public ConcurrentAccessException() {
    }

    public ConcurrentAccessException(String message) {
        super(message);
    }

    public ConcurrentAccessException(String message, Throwable cause) {
        super(message, cause);
    }

    public ConcurrentAccessException(Throwable cause) {
        super(cause);
    }

    public ConcurrentAccessException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }
}
