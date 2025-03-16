module hr.vpetrina.starwars.rebellion {
    requires javafx.controls;
    requires javafx.fxml;
    requires static lombok;
    requires jdk.jshell;
    requires java.desktop;
    requires javafx.media;


    opens hr.vpetrina.starwars to javafx.fxml;
    exports hr.vpetrina.starwars;
    exports hr.vpetrina.starwars.controller;
    opens hr.vpetrina.starwars.controller to javafx.fxml;
}