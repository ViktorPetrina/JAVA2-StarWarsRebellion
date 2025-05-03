package hr.vpetrina.starwars.util;

import hr.vpetrina.starwars.exception.XmlParseException;
import hr.vpetrina.starwars.model.*;
import org.w3c.dom.*;
import org.xml.sax.ErrorHandler;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

public class XmlUtils {

    private XmlUtils() {}

    private static final String DOCTYPE = "DOCTYPE";
    private static final String DTD = "dtd/gameMoves.dtd";
    private static final String GAME_MOVES = "GameMoves";
    private static final String FILENAME = "xml/gameMoves.xml";

    public static void saveNewMove(GameMove gameMove)
    {
        List<GameMove> gameMoveList = null;
        try {
            gameMoveList = loadGameMoves();
            Document document = createDocument(GAME_MOVES);

            if(gameMoveList.isEmpty()) {
                appendGameMoveElement(gameMove, document);
            }
            else {
                gameMoveList.add(gameMove);
                for(GameMove nextGameMove : gameMoveList) {
                    appendGameMoveElement(nextGameMove, document);
                }
            }

            saveDocument(document, FILENAME);

        } catch (ParserConfigurationException | TransformerException e) {
            throw new XmlParseException("An error occured while saving XML move", e);
        }

    }

    public static List<GameMove> loadGameMoves() {
        return parse(FILENAME);
    }

    private static List<GameMove> parse(String path) {

        if(!Files.exists(Path.of(path))) {
            return new ArrayList<>();
        }

        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        factory.setValidating(true);
        DocumentBuilder builder = null;

        try {
            builder = factory.newDocumentBuilder();
        } catch (ParserConfigurationException e) {
            throw new XmlParseException("An error occurred while parsing game moves!", e);
        }

        builder.setErrorHandler(new ErrorHandler() {
            @Override
            public void warning(SAXParseException exception) throws SAXException {
                throw new XmlParseException("A warning occurred while building XML with game moves!", exception);
            }

            @Override
            public void error(SAXParseException exception) throws SAXException {
                throw new XmlParseException("An error occurred while building XML with game moves!", exception);
            }

            @Override
            public void fatalError(SAXParseException exception) throws SAXException {
                throw new XmlParseException("A fatal error occurred while building XML with game moves!", exception);
            }
        });

        Document document = null;

        try {
            document = builder.parse(new File(path));
        } catch (SAXException | IOException e) {
            throw new XmlParseException("An error occurred while parsing games moves in XML format!", e);
        }

        return retrieveGameMoves(document);
    }

    private static List<GameMove> retrieveGameMoves(Document document) {
        List<GameMove> gameMoves = new ArrayList<>();
        Element documentElement = document.getDocumentElement();
        NodeList nodes = documentElement.getElementsByTagName(GameMoveTag.GAME_MOVE.getTagName());

        for (int i = 0; i < nodes.getLength(); i++) {
            Element item = (Element) nodes.item(i);

            Planet planet = new Planet(
                    item.getElementsByTagName(GameMoveTag.PLANET.getTagName()).item(0).getTextContent()
            );

            List<Leader> leaders = new ArrayList<>();
            NodeList leaderNodes = item.getElementsByTagName(GameMoveTag.LEADER.getTagName());
            for (int j = 0; j < leaderNodes.getLength(); j++) {
                leaders.add(new Leader(leaderNodes.item(j).getTextContent()));
            }

            MoveType moveType = MoveType.valueOf(
                    item.getElementsByTagName(GameMoveTag.MOVE_TYPE.getTagName()).item(0).getTextContent()
            );

            Faction winner = null;
            if (moveType == MoveType.ATTACK) {
                Node winnerNode = item.getElementsByTagName(GameMoveTag.WINNER.getTagName()).item(0);
                if (winnerNode != null) {
                    winner = Faction.valueOf(winnerNode.getTextContent());
                }
            }

            Faction executor = Faction.valueOf(
                    item.getElementsByTagName(GameMoveTag.EXECUTOR.getTagName()).item(0).getTextContent()
            );

            GameMove gameMove = new GameMove(planet, leaders, moveType, winner, executor);
            gameMoves.add(gameMove);
        }

        return gameMoves;
    }

    private static Document createDocument(String element) throws ParserConfigurationException {
        DocumentBuilder builder = DocumentBuilderFactory.newInstance().newDocumentBuilder();
        DOMImplementation dom = builder.getDOMImplementation();
        DocumentType docType = dom.createDocumentType(DOCTYPE, null, DTD);
        return dom.createDocument(null, element, docType);
    }

    private static void saveDocument(Document document, String filename) throws TransformerException {
        Transformer transformer = TransformerFactory.newDefaultInstance().newTransformer();
        transformer.setOutputProperty(OutputKeys.INDENT, "yes");
        transformer.setOutputProperty(OutputKeys.DOCTYPE_SYSTEM, document.getDoctype().getSystemId());
        transformer.transform(new DOMSource(document), new StreamResult(new File(filename)));
    }

    private static void appendGameMoveElement(GameMove gameMove, Document document) {
        Element gameMoveElement = document.createElement(GameMoveTag.GAME_MOVE.getTagName());
        document.getDocumentElement().appendChild(gameMoveElement);

        gameMoveElement.appendChild(createElement(document, GameMoveTag.PLANET.getTagName(), gameMove.getPlanet().getName()));

        Element leadersElement = document.createElement(GameMoveTag.LEADERS.getTagName());
        for (Leader leader : gameMove.getLeaders()) {
            leadersElement.appendChild(createElement(document, GameMoveTag.LEADER.getTagName(), leader.getName()));
        }
        gameMoveElement.appendChild(leadersElement);

        gameMoveElement.appendChild(createElement(document, GameMoveTag.MOVE_TYPE.getTagName(), gameMove.getMoveType().name()));

        if ((gameMove.getMoveType() == MoveType.ATTACK || gameMove.getMoveType() == MoveType.SEARCH)
                && gameMove.getWinner() != null) {
            gameMoveElement.appendChild(createElement(document, GameMoveTag.WINNER.getTagName(), gameMove.getWinner().name()));
        }

        gameMoveElement.appendChild(createElement(document, GameMoveTag.EXECUTOR.getTagName(), gameMove.getExecutor().name()));
    }

    private static Node createElement(Document document, String tagName, String data) {
        Element element = document.createElement(tagName);
        Text text = document.createTextNode(data);
        element.appendChild(text);
        return element;
    }
}
