package hr.vpetrina.starwars.exception;

public class FileAccessException extends RuntimeException {

    public FileAccessException() {
        super();
    }

    public FileAccessException(String message) {
        super(message);
    }

    public FileAccessException(String message, Throwable cause) {
        super(message, cause);
    }

    public FileAccessException(Throwable cause) {
        super(cause);
    }

    protected FileAccessException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }
}
