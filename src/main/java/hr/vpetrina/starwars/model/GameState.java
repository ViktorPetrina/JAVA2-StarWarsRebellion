package hr.vpetrina.starwars.model;

import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;
import java.util.List;

@Getter
public class GameState implements Serializable {

    private final Planet secretBaseLocation;
    private final Faction factionTurn;
    private final int currentTurn;
    private final int rebelReputation;
    private final Planet searchingPlanet;
    private final List<Leader> rebelLeaders;
    private final List<Leader> empireLeaders;

    private GameState(
            Faction turn,
            Planet secretBaseLocation,
            int currentTurn,
            int rebelReputation,
            Planet searchingPlanet, List<Leader> opponentLeaders, List<Leader> empireLeaders
    ) {
        this.secretBaseLocation = secretBaseLocation;
        this.factionTurn = turn;
        this.currentTurn = currentTurn;
        this.rebelReputation = rebelReputation;
        this.searchingPlanet = searchingPlanet;
        this.rebelLeaders = opponentLeaders;
        this.empireLeaders = empireLeaders;
    }

    @Getter @Setter
    private static Planet secretBaseLocationStatic;
    @Getter @Setter
    private static Planet searchingPlanetStatic;
    @Getter @Setter
    private static Faction playerFaction;
    @Getter @Setter
    private static List<Leader> playerLeaders;
    @Getter @Setter
    private static Player currentPlayer;
    @Getter @Setter
    private static Faction factionTurnStatic = Faction.REBELLION;
    @Getter @Setter
    private static int currentTurnStatic = 0;
    @Getter @Setter
    private static int rebelReputationStatic = 13;
    @Getter @Setter
    private static List<Leader> rebelLeadersStatic;
    private static List<Leader> empireLeadersStatic;


    public static GameState getGameState() {
        return new GameState(
                factionTurnStatic,
                secretBaseLocationStatic,
                currentTurnStatic,
                rebelReputationStatic,
                searchingPlanetStatic,
                rebelLeadersStatic,
                empireLeadersStatic
        );
    }
}
