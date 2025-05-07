package hr.vpetrina.starwars.model;

import lombok.AllArgsConstructor;
import lombok.Data;

import java.io.Serializable;
import java.util.Objects;

@AllArgsConstructor
@Data
public class Leader implements Serializable {
    private String name;
    private Faction faction;
    private Stats skills;
    private Health health;
    private Planet location;
    private Boolean isCaptured;
    private String imagePath;

    public Leader(String name) {
        this.name = name;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || getClass() != obj.getClass()) return false;
        Leader other = (Leader) obj;
        return Objects.equals(name, other.name);
    }

    @Override
    public int hashCode() {
        return Objects.hash(name);
    }
}

