package hr.vpetrina.starwars.util;

import hr.vpetrina.starwars.model.Faction;
import hr.vpetrina.starwars.model.GameMove;

public class MessageUtils {

    private MessageUtils() {}

    public static String getIndexedEventMessage(int moveIndex, GameMove gameMove) {
        return "Move " + (moveIndex + 1) + ":\n" + getEventMessage(gameMove) + "\n";
    }

    public static String getEventMessage(GameMove gameMove) {
        return switch (gameMove.getMoveType()) {
            case ATTACK -> handleAttack(gameMove);
            case SEARCH -> handleSearch(gameMove);
            case DEPLOY -> handleDeploy(gameMove);
            case SECRET_BASE_PICK -> "The REBELLION chose a planet for its secret base.";
        };
    }

    private static String handleAttack(GameMove gameMove) {
        Faction executor = gameMove.getExecutor();
        Faction opponent = (executor == Faction.EMPIRE) ? Faction.REBELLION : Faction.EMPIRE;
        Faction winner = gameMove.getWinner();
        String planetName = gameMove.getPlanet().getName();
        StringBuilder text = new StringBuilder();

        text.append(executor.name())
                .append(" attacked ")
                .append(opponent.name())
                .append(" on ")
                .append(planetName)
                .append(". ");

        if (executor == winner) {
            if (!gameMove.getLeaders().isEmpty()) {
                text.append("They won and captured ")
                        .append(gameMove.getLeaders().getFirst().getName())
                        .append(".");
            } else {
                text.append("They won.");
            }
        } else {
            text.append("They lost the battle.");
        }
        return text.toString();
    }

    private static String handleSearch(GameMove gameMove) {
        Faction winner = gameMove.getWinner();
        String planetName = gameMove.getPlanet().getName();
        StringBuilder text = new StringBuilder();

        text.append("The EMPIRE searched ")
                .append(planetName)
                .append(" for the rebel base. ");

        if (winner == Faction.EMPIRE) {
            text.append("They found the base!");
        } else {
            text.append("They did not find the base.");
        }
        return text.toString();
    }

    private static String handleDeploy(GameMove gameMove) {
        Faction executor = gameMove.getExecutor();
        String planetName = gameMove.getPlanet().getName();
        StringBuilder text = new StringBuilder();

        if (!gameMove.getLeaders().isEmpty()) {
            text.append(executor.name())
                    .append(" deployed ")
                    .append(gameMove.getLeaders().getFirst().getName())
                    .append(" to ")
                    .append(planetName)
                    .append(".");
        } else {
            text.append(executor.name())
                    .append(" deployed to ")
                    .append(planetName)
                    .append(".");
        }
        return text.toString();
    }
}

