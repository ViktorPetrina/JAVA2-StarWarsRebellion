package hr.vpetrina.starwars.util;

import hr.vpetrina.starwars.model.*;

import java.util.List;
import java.util.Random;
import java.util.concurrent.atomic.AtomicReference;

public class CombatUtils {

    private CombatUtils() {}

    public static CombatResult attackPlanet(Planet planet) {
        var result = CombatResult.NO_LEADERS;

        if (planet != null && !GameUtils.hasFactionLeaders(planet, Faction.EMPIRE)) {
            return result;
        }

        var executorFaction = GameState.getPlayerFactionStatic();

        if (planet != null && !planet.getLeaders().isEmpty()) {
            var removedLeader = CombatUtils.initiateCombat(planet);
            removeLeaderFromPlanet(removedLeader, planet);

            if (removedLeader.getFaction().equals(Faction.REBELLION)) {

                var gameMove = new GameMove(planet, MoveType.ATTACK, Faction.EMPIRE, executorFaction);
                gameMove.getLeaders().add(removedLeader);
                XmlUtils.saveNewMove(gameMove);
                ThreadUtils.saveLastEvent(gameMove);

                result = CombatResult.SUCCESS;
            }
            else {

                var gameMove = new GameMove(planet, MoveType.ATTACK, Faction.REBELLION, executorFaction);
                XmlUtils.saveNewMove(gameMove);
                ThreadUtils.saveLastEvent(gameMove);

                result = CombatResult.FAILURE;
            }

            return result;
        }

        return result;
    }

    private static void removeLeaderFromPlanet(Leader leader, Planet planet) {
        planet.getLeaders().removeIf(l -> l.getName().equals(leader.getName()));
        GameState.getRebelLeadersStatic().removeIf(l -> l.getName().equals(leader.getName()));
        leader.setLocation(null);
    }

    private static Leader initiateCombat(Planet planet) {
        GameState.setSearchingPlanetStatic(planet);
        Leader removedLeader;
        if (Faction.REBELLION.equals(CombatUtils.doCombat(planet.getLeaders()))) {
            GameUtils.reputationDown();
            removedLeader = GameUtils.captureLeader(planet, Faction.EMPIRE);
        }
        else {
            removedLeader = GameUtils.captureLeader(planet, Faction.REBELLION);
        }
        return removedLeader;
    }

    private static Faction doCombat(List<Leader> leaders) {
        Faction winner;

        AtomicReference<Integer> empireSkills = new AtomicReference<>(0);
        AtomicReference<Integer> rebellionSkills = new AtomicReference<>(0);

        leaders.forEach(leader -> {
            if (leader.getFaction() == Faction.REBELLION) {
                rebellionSkills.updateAndGet(v -> v + leader.getSkills().getCombat());
            }
            else {
                empireSkills.updateAndGet(v -> v + leader.getSkills().getCombat());
            }
        });

        int rebellion = rollDice(rebellionSkills.get());
        int empire = rollDice(empireSkills.get());

        if (empire > rebellion) {
            winner = Faction.EMPIRE;
        }
        else {
            winner = Faction.REBELLION;
        }

        return winner;
    }

    private static Integer rollDice(Integer count) {
        Random random = new Random();

        int roll = 0;
        for (int i = 0; i < count; i++) {
            roll += random.nextInt(10) + 1;
        }

        return roll;
    }
}
