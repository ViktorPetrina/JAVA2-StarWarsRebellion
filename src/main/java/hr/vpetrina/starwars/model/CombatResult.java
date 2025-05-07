package hr.vpetrina.starwars.model;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

@AllArgsConstructor
@Getter
@Setter
public class CombatResult {
    CombatOutcome outcome;
    Leader capturedLeader;
}
