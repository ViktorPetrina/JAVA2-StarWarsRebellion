package hr.vpetrina.starwars.util;

import java.io.IOException;
import java.lang.reflect.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.stream.Stream;

public class DocumentationUtils {

    private DocumentationUtils() {}

    private static final String PATH_WITH_CLASSES = "target/classes/";
    private static final String HTML_DOCUMENTATION_FILE_NAME = "doc/documentation.html";
    private static final String CLASS_FILE_NAME_EXTENSION = ".class";
    public static final String UNORDERED_LIST_END = "</ul>";
    public static final String LIST_ITEM_END = "</p></li>";

    public static void generateDocumentation() throws IOException {

        Path start = Paths.get(PATH_WITH_CLASSES);
        try (Stream<Path> stream = Files.walk(start, Integer.MAX_VALUE)) {
            List<String> classList = stream
                    .filter(f -> f.getFileName().toString().endsWith(CLASS_FILE_NAME_EXTENSION)
                            && Character.isUpperCase(f.getFileName().toString().charAt(0)))
                    .map(String::valueOf)
                    .sorted()
                    .toList();

            String htmlString = generateHtmlDocumentationCode(classList);

            Files.writeString(Path.of(HTML_DOCUMENTATION_FILE_NAME), htmlString);
        }
    }

    private static String generateHtmlDocumentationCode(List<String> classList) {
        StringBuilder htmlBuilder = new StringBuilder();
        htmlBuilder.append(HEADER);

        for (String className : classList) {
            appendClassDocumentation(htmlBuilder, className);
        }

        htmlBuilder.append(FOOTER);
        return htmlBuilder.toString();
    }

    private static void appendClassDocumentation(StringBuilder htmlBuilder, String classPath) {
        String className = normalizeClassName(classPath);
        try {
            Class<?> clazz = Class.forName(className);
            String shortClassName = className.substring(className.lastIndexOf('.') + 1);
            htmlBuilder.append("<li><p class=\"class-name\">").append(shortClassName).append("</p>");

            appendConstructors(htmlBuilder, clazz);
            appendFields(htmlBuilder, clazz);
            appendMethods(htmlBuilder, clazz);

            htmlBuilder.append("</li>");
        } catch (ClassNotFoundException e) {
            LogUtils.logSevere("Error while appending class name: " + e.getMessage());
        }
    }

    private static String normalizeClassName(String classPath) {
        return classPath
                .substring(PATH_WITH_CLASSES.length(), classPath.length() - CLASS_FILE_NAME_EXTENSION.length())
                .replace("\\", ".");
    }

    private static void appendConstructors(StringBuilder builder, Class<?> clazz) {
        Constructor<?>[] constructors = clazz.getConstructors();
        if (constructors.length == 0) {
            builder.append("<p>No constructor</p>");
            return;
        }
        builder.append("<p>List of constructors:</p><ul class=\"constructor-list\">");
        for (Constructor<?> constructor : constructors) {
            String name = constructor.getName().substring(constructor.getName().lastIndexOf('.') + 1);
            builder.append("<li><p class=\"constructor-item\">").append(name).append(LIST_ITEM_END);
        }
        builder.append(UNORDERED_LIST_END);
    }

    private static void appendFields(StringBuilder builder, Class<?> clazz) {
        Field[] fields = clazz.getDeclaredFields();
        if (fields.length == 0) {
            builder.append("<p>No fields</p>");
            return;
        }
        builder.append("<p>List of fields:</p><ul class=\"field-list\">");
        for (Field field : fields) {
            String modifiers = Modifier.toString(field.getModifiers());
            builder.append("<li><p class=\"field-item\">")
                    .append(modifiers).append(" ")
                    .append(field.getName())
                    .append(LIST_ITEM_END);
        }
        builder.append(UNORDERED_LIST_END);
    }

    private static void appendMethods(StringBuilder builder, Class<?> clazz) {
        Method[] methods = clazz.getMethods();
        if (methods.length == 0) {
            builder.append("<p>No methods</p>");
            return;
        }
        builder.append("<p>List of methods:</p><ul class=\"method-list\">");
        for (Method method : methods) {
            builder.append("<li><p class=\"method-item\">")
                    .append(method.getName())
                    .append(getMethodParams(method))
                    .append(LIST_ITEM_END);
        }
        builder.append(UNORDERED_LIST_END);
    }

    private static StringBuilder getMethodParams(Method method) {
        StringBuilder params = new StringBuilder("(");
        Parameter[] parameters = method.getParameters();
        for (int i = 0; i < parameters.length; i++) {
            params.append(parameters[i].getType().getSimpleName());
            if (i < parameters.length - 1) {
                params.append(", ");
            }
        }
        return params.append(")");
    }

    private static final String HEADER =
        """
    <!DOCTYPE html>
    <html>
    <head>
    <title>Star Wars: Rebellion documentation</title>
    <style>
        body {
            background-color: black;
            color: white;
            font-family: Arial, sans-serif;
        }
        p {
            margin: 10px 0;
        }
        .class-list-heading {
            font-size: 24px;
            font-weight: bold;
            color: #FFD700;
        }
        .class-name {
            font-size: 20px;
            font-weight: bold;
            color: #FFD700;
        }
        .constructor-list, .field-list, .method-list {
            margin-left: 20px;
        }
        .constructor-item, .field-item, .method-item {
            color: #ADD8E6;
        }
    </style>
    </head>
    <body>
    <p class="class-list-heading">List of classes:</p>
    <ul>""";

    private static final String FOOTER =
        """
    </ul>
    </body>
    </html>""";
}
