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
    private final int currentTurn;
    private final int rebelReputation;
    private final Planet searchingPlanet;
    private final List<Leader> rebelLeaders = new ArrayList<>();
    private final List<Leader> empireLeaders = new ArrayList<>();

    private GameState(
            Faction turn,
            Planet secretBaseLocation,
            int currentTurn,
            int rebelReputation,
            Planet searchingPlanet, List<Leader> _rebelLeaders, List<Leader> _empireLeaders
    ) {
        this.secretBaseLocation = secretBaseLocation;
        this.factionTurn = turn;
        this.currentTurn = currentTurn;
        this.rebelReputation = rebelReputation;
        this.searchingPlanet = searchingPlanet;
        rebelLeaders.addAll(_rebelLeaders);
        empireLeaders.addAll(_empireLeaders);
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
                empireLeadersStatic
        );
    }
}
