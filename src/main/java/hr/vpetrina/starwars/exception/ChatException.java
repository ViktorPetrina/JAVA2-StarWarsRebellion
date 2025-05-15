package hr.vpetrina.starwars.exception;

public class ChatException extends RuntimeException {
    public ChatException() {
    }

    public ChatException(String message) {
        super(message);
    }

    public ChatException(String message, Throwable cause) {
        super(message, cause);
    }

    public ChatException(Throwable cause) {
        super(cause);
    }

    public ChatException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }
}
