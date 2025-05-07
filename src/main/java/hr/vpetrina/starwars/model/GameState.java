package hr.vpetrina.starwars.model;

import hr.vpetrina.starwars.util.GameUtils;
import lombok.Getter;
import lombok.Setter;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

@Getter
public class GameState implements Serializable {

    private final Planet secretBaseLocation;
    private final Faction factionTurn;
    private final Integer currentTurn;
    private final Integer rebelReputation;
    private final Planet searchingPlanet;
    private final List<Leader> rebelLeaders = new ArrayList<>();
    private final List<Leader> empireLeaders = new ArrayList<>();
    private final Faction playerFaction;
    private final List<Leader> playerLeaders = new ArrayList<>();
    private final Leader capturedLeader;

    private GameState(
            Faction turn,
            Planet secretBaseLocation,
            int currentTurn,
            int rebelReputation,
            Planet searchingPlanet,
            List<Leader> rebelLeaders,
            List<Leader> empireLeaders,
            Faction playerFaction,
            List<Leader> playerLeaders,
            Leader capturedLeader
    ) {
        this.secretBaseLocation = secretBaseLocation;
        this.factionTurn = turn;
        this.currentTurn = currentTurn;
        this.rebelReputation = rebelReputation;
        this.searchingPlanet = searchingPlanet;
        this.capturedLeader = capturedLeader;
        this.rebelLeaders.addAll(rebelLeaders);
        this.empireLeaders.addAll(empireLeaders);
        this.playerFaction = playerFaction;
        this.playerLeaders.addAll(playerLeaders);
    }

    @Getter @Setter
    private static Planet secretBaseLocationStatic;
    @Getter @Setter
    private static Planet searchingPlanetStatic;
    @Getter @Setter
    private static Faction playerFactionStatic;
    @Getter @Setter
    private static List<Leader> playerLeadersStatic;
    @Getter @Setter
    private static Player currentPlayer;
    @Getter @Setter
    private static Leader capturedLeaderStatic;
    @Getter @Setter
    private static Faction factionTurnStatic = Faction.REBELLION;
    @Getter @Setter
    private static List<Leader> rebelLeadersStatic = new ArrayList<>();
    @Getter @Setter
    private static List<Leader> empireLeadersStatic = new ArrayList<>();

    public static GameState getGameState() {
        return new GameState(
                factionTurnStatic,
                secretBaseLocationStatic,
                GameUtils.getCurrentTurn(),
                GameUtils.getRebelReputation(),
                searchingPlanetStatic,
                rebelLeadersStatic,
                empireLeadersStatic,
                playerFactionStatic,
                playerLeadersStatic,
                capturedLeaderStatic
        );
    }
}
