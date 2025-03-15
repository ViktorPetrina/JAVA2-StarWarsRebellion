package hr.vpetrina.starwars.util;

import java.io.IOException;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
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

        String htmlStart = """
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

        htmlBuilder.append(htmlStart);

        for (String className : classList) {
            className = className
                    .substring(PATH_WITH_CLASSES.length(), className.length() - CLASS_FILE_NAME_EXTENSION.length())
                    .replace("\\", ".");

            try {
                Class<?> clazz = Class.forName(className);
                String shortClassName = className.substring(className.lastIndexOf('.') + 1);
                htmlBuilder.append("<li><p class=\"class-name\">").append(shortClassName).append("</p>");

                if (clazz.getConstructors().length > 0) {
                    htmlBuilder.append("<p>List of constructors:</p>");
                    htmlBuilder.append("<ul class=\"constructor-list\">");

                    for (Constructor<?> constructor : clazz.getConstructors()) {
                        String constructorName = constructor.getName()
                                .substring(constructor.getName().lastIndexOf('.') + 1);
                        htmlBuilder.append("<li><p class=\"constructor-item\">")
                                .append(constructorName)
                                .append("</p></li>");
                    }

                    htmlBuilder.append("</ul>");
                } else {
                    htmlBuilder.append("<p>No constructor</p>");
                }

                Field[] fields = clazz.getDeclaredFields();
                if (fields.length > 0) {
                    htmlBuilder.append("<p>List of fields:</p>");
                    htmlBuilder.append("<ul class=\"field-list\">");

                    for (Field field : fields) {
                        String fieldName = field.getName();
                        String modifiers = Modifier.toString(field.getModifiers());
                        htmlBuilder.append("<li><p class=\"field-item\">")
                                .append(modifiers).append(" ")
                                .append(fieldName)
                                .append("</p></li>");
                    }

                    htmlBuilder.append("</ul>");
                } else {
                    htmlBuilder.append("<p>No fields</p>");
                }

                if (clazz.getMethods().length > 0) {
                    htmlBuilder.append("<p>List of methods:</p>");
                    htmlBuilder.append("<ul class=\"method-list\">");

                    for (Method method : clazz.getMethods()) {
                        String methodName = method.getName();
                        StringBuilder params = getStringBuilder(method);
                        htmlBuilder.append("<li><p class=\"method-item\">")
                                .append(methodName)
                                .append(params)
                                .append("</p></li>");
                    }

                    htmlBuilder.append("</ul>");
                } else {
                    htmlBuilder.append("<p>No methods</p>");
                }

                htmlBuilder.append("</li>");
            } catch (ClassNotFoundException e) {
                throw new RuntimeException(e);
            }
        }

        String htmlEnd = """
        </ul>
        </body>
        </html>""";

        htmlBuilder.append(htmlEnd);

        return htmlBuilder.toString();
    }

    private static StringBuilder getStringBuilder(Method method) {
        Class<?>[] parameterTypes = method.getParameterTypes();
        StringBuilder params = new StringBuilder("(");

        for (int i = 0; i < parameterTypes.length; i++) {
            params.append(parameterTypes[i].getSimpleName());
            if (i < parameterTypes.length - 1) {
                params.append(", ");
            }
        }
        params.append(")");
        return params;
    }
}
