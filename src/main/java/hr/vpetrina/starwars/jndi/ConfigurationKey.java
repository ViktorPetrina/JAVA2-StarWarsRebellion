package hr.vpetrina.starwars.jndi;

import lombok.Getter;

@Getter
public enum ConfigurationKey {

    PLAYER_1_SERVER_PORT("player.one.server.port"),
    PLAYER_2_SERVER_PORT("player.two.server.port"),
    HOSTNAME("host.name"),
    RMI_PORT("rmi.server.port");

    private final String key;

    ConfigurationKey(final String key) {
        this.key = key;
    }
}
