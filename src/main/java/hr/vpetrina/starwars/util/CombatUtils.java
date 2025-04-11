package hr.vpetrina.starwars.util;

import hr.vpetrina.starwars.model.Faction;
import hr.vpetrina.starwars.model.Leader;

import java.util.List;
import java.util.Random;
import java.util.concurrent.atomic.AtomicReference;

public class CombatUtils {

    private CombatUtils() {}

    public static Faction doCombat(List<Leader> leaders) {
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
