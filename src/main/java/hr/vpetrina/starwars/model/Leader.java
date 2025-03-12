package hr.vpetrina.starwars.model;

import lombok.AllArgsConstructor;
import lombok.Data;

@AllArgsConstructor
@Data
public class Leader {
    private String name;
    private Faction faction;
    private Stats skills;
    private Health health;
    private Planet location;
    private Boolean isCaptured;
    private String imagePath;
}

