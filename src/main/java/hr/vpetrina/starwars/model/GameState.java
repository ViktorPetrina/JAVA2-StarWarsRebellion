package hr.vpetrina.starwars.model;

public class GameState {

    private GameState() {}

    private static Faction playerOneFaction;
    private static Faction playerTwoFaction;

    public static Faction getPlayerOneFaction() {
        return playerOneFaction;
    }

    public static void setPlayerOneFaction(Faction playerOneFaction) {
        GameState.playerOneFaction = playerOneFaction;
    }

    public static Faction getPlayerTwoFaction() {
        return playerTwoFaction;
    }

    public static void setPlayerTwoFaction(Faction playerTwoFaction) {
        GameState.playerTwoFaction = playerTwoFaction;
    }
}
