port module Parser exposing (thirdParse
    , parseMeta
    , parseMessages
    , findWord
    , getTime 
    , getDate
    , histogram
    , gamalUsrRegex
    , lucyUsrRegex
    , stringWordCount
    , makeRectangles
    , main
    , s, t)

import HtmlParser as HtmlParser exposing (..)
import Regex exposing (..)
import Date exposing (..)
import Chart exposing (..)
import Html exposing (Html)
import Dict exposing (..)
import Collage exposing (..)
import Color exposing (..)
import Element exposing (..)
import Html.Attributes as Attr exposing (..)
import Text exposing (..)
import Mouse exposing (..)
import Html.Events exposing (..)
import Keyboard exposing (..)
-----------------------------------------------------------------------
type alias Message = 
    { user : String
    , meta : String
    , text : String
    }

type alias MultimediaMessage = 
    { user : String
    , meta : Meta 
    , msgs : List Node 
    }

type alias Meta = -- eventually we won't need this and can just use Date
    { zone : String --We can deal with this later
    , date : Date.Date
    }

type alias ParsedMessage =
    { user : String
    , meta : Meta
    , text : String
    }

type alias Word = String
type alias Freqencies = (Int, Int)

type alias BarLabel = (Int, Int, String, String)
{-Year, month, word, user-}
type alias Freq = Int

------------------------------------------------------------------------
{-parsing strings for messages-}
------------------------------------------------------------------------
{-parse text from list of nodes-}
thirdParse : List Node -> Maybe ParsedMessage
thirdParse ns = case ns of 
    (Element "div" [("class","message")] 
        ([Element "div" [("class","message_header")] 
            ([Element "span" 
                [("class","user")] 
                ([Text usr]),
                Element "span" [("class","meta")] 
                ([Text mta])])]))
    ::(Element "p" [] ([Text txt]))::rest -> 
        Just { user=usr, meta=parseMeta mta, text=txt }
    _ -> Nothing

{-parse multimedia from list of nodes // needs work-}
fourthParse : List Node -> Maybe MultimediaMessage
fourthParse ns = case ns of 
    (Element "div" [("class","message")] 
        ([Element "div" [("class","message_header")] 
            ([Element "span" [("class","user")] 
                ([Text usr]),
                Element "span" [("class","meta")]
                ([Text mta])])]))
    ::(Element "p" [] (nds))::rest -> 
        Just { user=usr, meta=parseMeta mta, msgs=nds }
    _ -> Nothing

{-Parse facebook's "meta" field for date info-}
parseMeta : String -> Meta
parseMeta m = 
    let 
        arr = split All (Regex.regex ", | ") m
    in 
        case arr of 
            [_,m,d,y,_,t,z] ->
            { zone = z
            , date = getDate (y,m,d,t)
            }
            _ -> Debug.crash "fail"

{-parse any node to look for messages // untested-}
parseFromTree : Node -> List ParsedMessage
parseFromTree t = case t of 
    Text _ -> []
    Comment _ -> []
    Element _ _ ns -> case parseMessagesFromNodes ns of 
        [] -> List.foldr (\x y -> List.append (parseFromTree x) y) [] ns 
        ms -> ms 

{-parse message a list of properly formatted nodes-}
parseMessagesFromNodes : List Node -> List ParsedMessage
parseMessagesFromNodes ns = case ns of 
    [] -> []
    [_] -> []
    x::y::rest -> case thirdParse ns of
        Just m -> m::parseMessagesFromNodes rest
        Nothing -> parseMessagesFromNodes (y::rest)

{-parse messages from a string containing only messages-}
parseMessages : String -> List ParsedMessage
parseMessages s = parseMessagesFromNodes (HtmlParser.parse s)

------------------------------------------------------------------------
{-filtering message of texts, finding words, etc.-}
------------------------------------------------------------------------

{-see if a string contains a given word-}
stringContainsWord : String -> String -> Bool
stringContainsWord s w = 
    let 
        arr = split All (Regex.regex "[,.?! ]") s
    in 
        List.member w arr 

{-count occurrances of a word in a string-}
stringWordCount : String -> String -> Int
stringWordCount s w = 
    let 
        arr = split All (Regex.regex "[,.?! ]") s
    in 
        List.foldr 
            (\x y -> if x==w then 1+y else y)
            0
            arr

{-filter list of parsed messages for the ones containing
 - the given string-}
findWord : String -> List ParsedMessage -> List ParsedMessage
findWord w ms = 
    List.filter (\m -> (stringContainsWord m.text w)) ms

{-make histogram of all words-}
makeHistogram : List String -> Dict Word Int
makeHistogram words = case words of
    word::words -> 
        let 
            dict = makeHistogram words
            count = get word dict
        in 
            case count of
                Just n  -> insert word (n+1) dict
                Nothing -> insert word 1 dict
    [] -> Dict.empty

{-make histogram from string-}
histogram : String -> Dict Word Int
histogram s = 
    let 
        msgs = parseMessages s 
        allWords = List.concat <| List.map (getWords) msgs
    in 
        makeHistogram allWords


------------------------------------------------------------------------
{-helpers-}
------------------------------------------------------------------------
---- bar label ----
getYearMonth : BarLabel -> (Int, Int)
getYearMonth l = case l of 
    (y,m,_,_) -> (y,m)

getWord : BarLabel -> String
getWord l = case l of 
    (_,_,w,_) -> w

getUser : BarLabel -> String 
getUser l = case l of 
    (_,_,_,u) -> u

orderBarLabel : (BarLabel, Freq) -> (BarLabel, Freq) -> Order
orderBarLabel (b1, _) (b2, _) = case (b1, b2) of 
    ((y1,m1,w1,u1),(y2,m2,w2,u2)) -> 
        if y1 < y2 then LT
        else if y1 == y2 && m1 < m2 then LT
        else if y1 == y2 && m1 == m2 && w1 < w2 then LT
        else if y1 == y2 && m1 == m2 && w1 == w2 && u1 < u2 then LT
        else if y1 == y2 && m1 == m2 && w1 == w2 && u1 == u2 then EQ
        else GT

---- date ----
monthToInt : Month -> Int
monthToInt m = case m of 
    Jan -> 1
    Feb -> 2 
    Mar -> 3
    Apr -> 4 
    May -> 5
    Jun -> 6
    Jul -> 7
    Aug -> 8
    Sep -> 9
    Oct -> 10
    Nov -> 11
    Dec -> 12

stringToMonthNumber : String -> String 
stringToMonthNumber s = case s of 
    "January"   -> "01"
    "February"  -> "02"
    "March"     -> "03"
    "April"     -> "04"
    "May"       -> "05"
    "June"      -> "06"
    "July"      -> "07"
    "August"    -> "08"
    "September" -> "09"
    "October"   -> "10"
    "November"  -> "11"
    "December"  -> "12"
    _           -> Debug.crash "fail"

getDate : (String, String, String, String) -> Date 
getDate (y, m, d, t) = 
    case Date.fromString (y++"-"++(stringToMonthNumber m)++"-"++(appendZeroToNumber d)++(getTime t)) of 
        Ok x -> x 
        _ -> Debug.crash "fail"

getTime : String -> String 
getTime t = 
    case String.split ":" t of 
        [h,m] -> 
            let 
                mins = String.left 2 m
                am = String.right 2 m 
                hrs = case String.toInt h of 
                    Ok x -> x 
                    _    -> Debug.crash "Fail"
                hours = 
                    if am == "am" then (appendZeroToNumber (toString hrs)) 
                    else if am == "pm" then toString (hrs + 12)
                    else Debug.crash "fail"
            in 
                "T" ++ hours ++ ":" ++ mins ++ ":00"
        _ -> Debug.crash "fail"

appendZeroToNumber : String -> String
appendZeroToNumber s = 
    case String.length s of 
        1 -> "0"++s
        2 -> s
        _ -> Debug.crash "fail"

---- color ----
medColors : List Color 
medColors = 
    [ Color.blue
    , Color.red
    , Color.green
    , Color.yellow
    , Color.brown
    , Color.purple
    , Color.orange ]

lightColors : List Color
lightColors = 
    [ Color.lightBlue
    , Color.lightRed
    , Color.lightGreen
    , Color.lightYellow
    , Color.lightBrown
    , Color.lightPurple
    , Color.lightOrange ]

darkColors : List Color
darkColors = 
    [ Color.darkBlue
    , Color.darkRed
    , Color.darkGreen
    , Color.darkYellow
    , Color.darkBrown
    , Color.darkPurple
    , Color.darkOrange ]

---- freq ----
updateFreq : Int -> Maybe Freq -> Maybe Freq
updateFreq n b = case b of 
    Nothing -> Debug.crash "updateFreq: nothing (shouldn't happen)" 
    Just hd -> Just (hd+n)

---- lists (lol) ----
findIndex : comparable -> List comparable -> Int
findIndex x xs = 
    case xs of 
        []      -> Debug.crash "findIndex: Not found (shouldn't happen)"
        x_::xs_ -> if x == x_ then 0 else 1 + (findIndex x xs_)

findElt : Int -> List a -> a
findElt n xs = 
    case xs of 
        []     -> Debug.crash "findElt: Oob (shouldn't happen)"
        x::xs_ -> if n == 0 then x else findElt (n-1) xs_

getWords : ParsedMessage -> List String
getWords pm = 
    let words = pm.text in
    split All (Regex.regex "[,.?! ]") words

----------------------------------------------------------------------------
{-making graph with the graphics library-}
----------------------------------------------------------------------------

{-given list of parsed messages, return a dictionary of (BarLabel, Freq)
 -with frequencies of each word for each user and (month,year) combination-}
makeBarInfos : List ParsedMessage -> String -> Dict BarLabel Freq 
makeBarInfos msgs s = case msgs of 
    [] -> Dict.empty
    hd::rest -> 
        let 
            d = makeBarInfos rest s
            date = hd.meta.date
            e = (Date.year date, monthToInt (Date.month date), s, hd.user)
            n = stringWordCount hd.text s
        in 
            if (Dict.member e d) then
                (Dict.update e (updateFreq n) d)
            else Dict.insert e n d

makeBarLabelList : List String -> String -> List (BarLabel, Freq)
makeBarLabelList ws s = 
    let 
        ms = parseMessages s
        ds = List.map (makeBarInfos ms) ws 
        ls = List.foldr (\x y -> (Dict.toList x) ++ y) [] ds
    in 
        ls

------------------------------------------------------------------------------------------
{-graphics-}
------------------------------------------------------------------------------------------

{- make one rectangle with color depending on user and string -}
makeRectangle : Float -> Int -> Regex -> Regex -> List String -> BarLabel -> Freq -> Float -> Form
makeRectangle width maxFreq usr1 usr2 strs lbl fq scl = 
    case lbl of 
        (y,m,w,u) -> 
            let 
                shade = if Regex.contains usr1 u then darkColors 
                else if Regex.contains usr2 u then lightColors
                else medColors
                index = (findIndex w strs) % 7 -- number of colors per shade
                color = findElt index shade
                rekt  = Collage.filled color (Collage.rect  width ((toFloat fq) * scl + (scl/2)))
                voff  = (toFloat (maxFreq - fq)) * scl / -2
                rect  = Collage.moveY voff rekt
                text  = Collage.text (Text.fromString (u ++ "." ++ w ++ ":" ++ (toString fq)))
                
                {-newline looks like a regular space-}
                obj   = Collage.group [rect, (Collage.moveY ((toFloat maxFreq) * (scl)/2 * -1) text)]
            in
                rekt

{-moveRectangles helper-}        
moveRectangles_ : Int -> Float -> List Form -> Bool -> List Form
moveRectangles_ n width rs horiz =
    let 
        dir = if horiz then Collage.moveX else Collage.moveY
    in  
        case rs of 
            [] -> []
            r::rs_ -> (dir ((toFloat n) * width) r) 
                      :: (moveRectangles_ (n+1) width rs_ horiz)

{-move rectangles to be spaced next to each other // needs work-}
moveRectangles : Float -> List Form -> List Form
moveRectangles width rs = moveRectangles_ 0 width rs True

{-make rectangles for a given string an list of parsed messages-}
makeRectangles : Float -> Float -> Regex -> Regex -> List String -> List ParsedMessage -> List Form
makeRectangles width scl usr1 usr2 strs ms = 
    let 
        ds = List.map (makeBarInfos ms) strs
        ls = List.map Dict.toList ds 
        l1 = List.foldr (++) [] ls
        l2 = List.sortWith orderBarLabel l1
        maxFreq = case List.maximum (List.map (\x -> (Tuple.second x)) l2) of 
            Nothing -> 0
            Just x -> x
        log = Debug.log (toString maxFreq)
        {-add in empties?-}
        l4 = List.map 
            (\x -> makeRectangle width maxFreq usr1 usr2 strs (Tuple.first x) (Tuple.second x) scl) 
            l2
        l5 = moveRectangles (width * 1.2) l4
    in
        l5

{-make rectangles into collage-}
makeCollage : Float -> List Form -> Element.Element
makeCollage width fs = 
    let 
        length = floor (width * (toFloat (List.length fs)) * 2.2) 
    in 
        Collage.collage length length fs 


---------------------------------------------------------------------------------
{-labels-}
---------------------------------------------------------------------------------
makeLabeledBox : String -> Color -> Form 
makeLabeledBox s c = 
    let 
        sq = Collage.filled c (Collage.rect 15 15)
        text  = Collage.moveX 30.0 (Collage.text (Text.fromString ("- " ++ s)))
        lb = Collage.group [sq,text]
    in 
        lb

makeKey_ : List String -> Int -> List Form 
makeKey_ strs n = 
    let 
        col = findElt (n%7) medColors
        strBoxes = case strs of 
            [] -> []
            s::rest -> (makeLabeledBox s col)::(makeKey_ rest (n+1))
    in 
        strBoxes

makeKey : String -> String -> List String -> Form
makeKey usr1 usr2 strs =
    let
        usr1box    = makeLabeledBox usr1 Color.darkGrey 
        usr2box    = makeLabeledBox usr2 Color.lightGrey
        wordboxes  = makeKey_ (List.sort strs) 0
        boxes      = wordboxes ++ [usr2box, usr1box]
        movedBoxes = moveRectangles_ 0 25 boxes False 
    in
        Collage.group movedBoxes  


---------------------------------------------------------------------------------
{-more time and date-}
---------------------------------------------------------------------------------

{- parse messages by frequency for a function on time -}
dateByNumberTime : List ParsedMessage -> (Date -> Int) -> Dict Int Int
dateByNumberTime msgs f = case msgs of 
    [] -> Dict.empty
    hd::rest -> 
        let 
            d = dateByNumberTime rest f
            date = f hd.meta.date
            g = Dict.get date d
        in 
        case g of
            Nothing -> Dict.insert date 1 d
            Just x  -> Dict.insert date (x+1) d

hourConvert : Int -> String
hourConvert i = case i of
    0 -> "12 am"
    1 -> "1 am"
    2 -> "2 am"
    3 -> "3 am"
    4 -> "4 am"
    5 -> "5 am"
    6 -> "6 am"
    7 -> "7 am"
    8 -> "8 am"
    9 -> "9 am"
    10 -> "10 am"
    11 -> "11 am"
    12 -> "12 pm"
    13 -> "1 pm"
    14 -> "2 pm"
    15 -> "3 pm"
    16 -> "4 pm"
    17 -> "5 pm"
    18 -> "6 pm"
    19 -> "7 pm"
    20 -> "8 pm"
    21 -> "9 pm"
    22 -> "10 pm"
    23 -> "11 pm"
    _  -> Debug.crash "Shouldn't happen"

m2i : Month -> Int
m2i m = case m of
    Jan -> 0
    Feb -> 1
    Mar -> 2
    Apr -> 3
    May -> 4
    Jun -> 5
    Jul -> 6
    Aug -> 7
    Sep -> 8
    Oct -> 9
    Nov -> 10
    Dec -> 11

monthConvert : Int -> String
monthConvert i = case i of
    0 -> "Jan"
    1 -> "Feb"
    2 -> "Mar"
    3 -> "Apr"
    4 -> "May"
    5 -> "Jun"
    6 -> "Jul"
    7 -> "Aug"
    8 -> "Sep"
    9 -> "Oct"
    10 -> "Nov"
    11 -> "Dec"
    _  -> Debug.crash "Shouldn't happen"

{-frequency of messages per hour-}
hours : List ParsedMessage -> List (String, Int)
hours s  = List.map 
        (\(x,y) -> (hourConvert x,y)) 
        (toList (dateByNumberTime s Date.hour))

{-frequency of messages per month-}
months : List ParsedMessage -> List (String, Int)
months s = List.map
        (\(x,y) -> (monthConvert x, y))
        (toList (dateByNumberTime s (\x -> m2i (Date.month x))))

{-frequency of messages per year-}
years : List ParsedMessage -> List (Int, Int)
years s = toList (dateByNumberTime s Date.year)

maybeApply : Maybe (List ParsedMessage) -> (List ParsedMessage -> b) -> b
maybeApply m f = case m of
    Nothing -> Debug.crash "We are leaving functional land so there are errors."
    Just x  -> f x

userMessages : List ParsedMessage -> Dict String (List ParsedMessage)
userMessages msgs = case msgs of 
    [] -> Dict.empty
    hd::rest -> 
        let 
            d = userMessages rest
            g = Dict.get hd.user d
        in 
        case g of
            Nothing -> Dict.insert hd.user [hd] d
            Just x  -> Dict.insert hd.user (hd::x) d

---------------------------------------------------------------------------------
{-model, view, update, main-}
---------------------------------------------------------------------------------

type alias Model = 
    { words : List String 
    , file : Maybe String
    , nextWord : String
    }

type Msg 
    = Word String 
    | File String
    | ResetWords
    | Reset
    | Enter
    | SendInfo --not using this
    | GraphInfo (List String) --not using this

initialModel : Model
initialModel = {words=[], file=Nothing, nextWord=""}

init : (Model, Cmd Msg)
init = (initialModel, Cmd.none)

-- We didnt' end up using this ---
port sendInfo : List (BarLabel, Freq) -> Cmd msg
port graphInfo : (List String -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions model = graphInfo GraphInfo
----------------------------------

update : Msg -> Model -> (Model, Cmd Msg) 
update msg model = 
    case msg of 
        Word w       -> ({model | nextWord = w}, Cmd.none)
        File f       -> ({model | file = Just f}, Cmd.none)
        ResetWords   -> ({model | words = []}, Cmd.none)
        Reset        -> (initialModel, Cmd.none)
        Enter        -> if String.isEmpty model.nextWord then (model, Cmd.none) 
        else ({model | words = model.nextWord::model.words, nextWord = ""}, Cmd.none)
        SendInfo     -> (model, sendInfo (makeBarLabelList model.words u))
        GraphInfo ws -> ({model | words = ws}, Cmd.none)



view : Model -> Html Msg 
view model = 
    let 
        resetWords = Html.button [Attr.class "button", onClick ResetWords] 
            [Html.text "Reset Words"] 
        reset      = Html.button [Attr.class "button", onClick Reset] 
            [Html.text "Reset"]
        enter      = Html.button [Attr.class "button", onClick Enter]
            [Html.text "Enter"]
        word       = Html.input 
            [Attr.class "input", type_ "word", placeholder "Enter word", onInput Word ] 
            []
        file       = Html.input 
            [Attr.class "button", type_ "file", placeholder "Enter file", onInput File ] 
            []
        display    = Html.div [Attr.class "largetext"]
            [Html.text ("Words: " ++ (toString (List.sort model.words)))]
        enterword  = Html.span [] [word, enter]
        resets     = Html.span [] [resetWords, reset]
        send       = Html.button [onClick SendInfo] [Html.text "Send"]
        graph      = Html.button [onClick (GraphInfo ["one", "two"])] [Html.text "Graph"]
        t          = "Graph of message frequncies between Lucy and Gamal in Aug. 2016"
        title      = Html.div [Attr.class "largetext"] [Html.text t]
        break      = Html.br [Attr.class "break"] []
        ms         = parseMessages u
        w          = 40
        rs         = makeRectangles w w lucyUsrRegex gamalUsrRegex model.words ms
        c          = makeCollage w rs
        k          = Collage.collage 100 100 [makeKey "Lucy" "Gamal" model.words]
        key        = Html.div [Attr.class "graph"] [Element.toHtml k]
        histTitle  = "histogram of some of the words in a sample of Facebook messages"
        histogram  = graphHistogram u histTitle
        collage    = Html.div [Attr.class "graph"] [Element.toHtml c]
        results    = Html.span [] [collage, key] 
        all        =
            Html.div 
                [] 
                [ enterword
                , resetWords
                , break
                , file
                , break
                , break
                , break
                , title
                , break
                , results
                , histogram ]
    in 
        all

main : Program Never Model Msg
main = Html.program  
    { init          = init
    , view          = view
    , update        = update
    , subscriptions = subscriptions
    }

-------------------------------------------------------------------------------
{-make charts using other libraries-}
-------------------------------------------------------------------------------

{-chart of frequencies for a given word-}
makeChart : String -> String -> List String -> Html a
makeChart s title xs = 
    let
        ps = parseMessages s
        ys = List.map (\x -> (toFloat (List.length (findWord x ps)), x)) xs
    in 
        Chart.toHtml (Chart.title title (Chart.vBar ys))

{-histogram, more or less-}
graphHistogram : String -> String -> Html a
graphHistogram s title = 
    let 
        d = List.sort (List.filter 
                (\x -> 
                    ((4 < String.length (Tuple.second x)) 
                        && (2 < Tuple.first x)) 
                    || (8 < Tuple.first x)) 
                (List.map 
                        (\x -> (toFloat (Tuple.second x), Tuple.first x))
                        (Dict.toList (Dict.remove "" (histogram s)))))
    in 
        Chart.toHtml (Chart.title title (Chart.vBar d))

--------------------------------------------------------------------------------
{-strings, etc. for testing-}
--------------------------------------------------------------------------------
gamalUsrRegex = Regex.regex "Gamal DeWeever|1278293023@facebook.com"
lucyUsrRegex = Regex.regex "Lucy Newman|100009794951641@facebook.com"
-- for some reason \. is causing problems? . matches any single char

d = "Sunday, November 20, 2016 at 2:16am CST"
s = """<div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Sunday, November 20, 2016 at 2:16am CST</span></div></div><p>Why is this so binary</p>"""
t = """<div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Sunday, November 20, 2016 at 2:16am CST</span></div></div><p>Why is this so binary</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Sunday, November 20, 2016 at 2:16am CST</span></div></div><p>Not rly sure what that means</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Sunday, November 20, 2016 at 2:16am CST</span></div></div><p>I did it with what I thought would be your answers and got &quot;next generation left&quot;</p><div class="message"><div class="message_header"><span class="user">100009794951641&#064;facebook.com</span><span class="meta">Sunday, November 20, 2016 at 2:15am CST</span></div></div><p>http://www.people-press.org/quiz/political-typology/</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Sunday, November 20, 2016 at 2:15am CST</span></div></div><p>Sure</p><div class="message"><div class="message_header"><span class="user">1278293023&#064;facebook.com</span><span class="meta">Sunday, November 20, 2016 at 1:03am CST</span></div></div><p>And it&#039;ll show you I&#039;m a libertarian</p><div class="message"><div class="message_header"><span class="user">1278293023&#064;facebook.com</span><span class="meta">Sunday, November 20, 2016 at 1:03am CST</span></div></div><p>It&#039;ll be fun to take</p><div class="message"><div class="message_header"><span class="user">1278293023&#064;facebook.com</span><span class="meta">Sunday, November 20, 2016 at 1:03am CST</span></div></div><p>Why don&#039;t you throw me a test</p><div class="message"><div class="message_header"><span class="user">1278293023&#064;facebook.com</span><span class="meta">Sunday, November 20, 2016 at 1:03am CST</span></div></div><p>3. You said so, and so do the political compasses</p><div class="message"><div class="message_header"><span class="user">1278293023&#064;facebook.com</span><span class="meta">Sunday, November 20, 2016 at 1:03am CST</span></div></div><p>2. Not handouts, premiums for living in this country.</p><div class="message"><div class="message_header"><span class="user">1278293023&#064;facebook.com</span><span class="meta">Sunday, November 20, 2016 at 1:02am CST</span></div></div><p>1. Leftovers</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Sunday, November 20, 2016 at 1:02am CST</span></div></div><p>3) I still don&#039;t understand how you consider yourself a libertarian.</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Sunday, November 20, 2016 at 1:01am CST</span></div></div><p>2) I thought you were advocating for handouts</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Sunday, November 20, 2016 at 1:01am CST</span></div></div><p>1) sharing with a friend is different from handouts</p><div class="message"><div class="message_header"><span class="user">1278293023&#064;facebook.com</span><span class="meta">Sunday, November 20, 2016 at 12:44am CST</span></div></div><p>I DONT NEED YOUR HANDOUTS MAN</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Sunday, November 20, 2016 at 12:40am CST</span></div></div><p>I&#039;m sure there will still be some left.</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Sunday, November 20, 2016 at 12:40am CST</span></div></div><p>You just have to like, tell me next time you come to Chicago</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Sunday, November 20, 2016 at 12:39am CST</span></div></div><p>Sure, theres extra cheesecake</p><div class="message"><div class="message_header"><span class="user">1278293023&#064;facebook.com</span><span class="meta">Sunday, November 20, 2016 at 12:39am CST</span></div></div><p>Sounds great</p><div class="message"><div class="message_header"><span class="user">1278293023&#064;facebook.com</span><span class="meta">Sunday, November 20, 2016 at 12:39am CST</span></div></div><p>Can I have some of that premium no hunger?</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Sunday, November 20, 2016 at 12:39am CST</span></div></div><p>I just ate way too much at the Cheesecake Factory.</p>"""
u = """<div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Friday, August 26, 2016 at 11:28am CDT</span></div></div><p>I am having my blood donated to and the gave me to a penguin to keep me company to to to.</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Friday, August 26, 2016 at 11:27am CDT</span></div></div><p>I was just curious to cause like you&#039;re the first to year of booth.</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Friday, August 26, 2016 at 11:24am CDT</span></div></div><p>Were they ppl from ihouse to or like just random ppl or what??</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Friday, August 26, 2016 at 11:24am CDT</span></div></div><p>Yeah, I guess.</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Friday, August 26, 2016 at 11:23am CDT</span></div></div><p>Wow.</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Friday, August 26, 2016 at 11:23am CDT</span></div></div><p>that was... nearly four years ago to </p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Friday, August 26, 2016 at 11:23am CDT</span></div></div><p>I just don&#039;t remember them</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Friday, August 26, 2016 at 11:23am CDT</span></div></div><p>Lol, I did</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Friday, August 26, 2016 at 11:21am CDT</span></div></div><p>Did you not go to oweek??</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Friday, August 26, 2016 at 11:21am CDT</span></div></div><p>???</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Friday, August 26, 2016 at 11:17am CDT</span></div></div><p>Lol, no idea</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Friday, August 26, 2016 at 12:35am CDT</span></div></div><p>Who were the oaides your year???</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Thursday, August 25, 2016 at 11:02pm CDT</span></div></div><p>🏄</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 11:02pm CDT</span></div></div><p>Surfs up dude</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 11:01pm CDT</span></div></div><p>󾟚</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 11:01pm CDT</span></div></div><p>Remember to exercise your privilege if you can</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 11:01pm CDT</span></div></div><p>Okay you go running. I&#039;ll take a nap</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Thursday, August 25, 2016 at 11:01pm CDT</span></div></div><p>Hopefully no one thinks of that idea before I publish it.</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Thursday, August 25, 2016 at 11:01pm CDT</span></div></div><p>Exactly.</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 11:00pm CDT</span></div></div><p>And then you can innovate</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Thursday, August 25, 2016 at 11:00pm CDT</span></div></div><p>I&#039;ll bring my kite with a key attached to it.</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Thursday, August 25, 2016 at 11:00pm CDT</span></div></div><p>Okay, sounds like a plan.</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Thursday, August 25, 2016 at 11:00pm CDT</span></div></div><p>Well in the future I will know whether it rains tonight or not so I won&#039;t have to say possibly.</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 10:59pm CDT</span></div></div><p>Like if you run today and you don&#039;t want to run in the future, you can say.. remember that one day you ran and it was hot and possibly raining?</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Thursday, August 25, 2016 at 10:59pm CDT</span></div></div><p>And possibly raining.</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Thursday, August 25, 2016 at 10:58pm CDT</span></div></div><p>But like, it&#039;s hot out.</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 10:58pm CDT</span></div></div><p>Oh then yeah probs</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Thursday, August 25, 2016 at 10:58pm CDT</span></div></div><p>Yeah, I usually run this late.</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Thursday, August 25, 2016 at 10:58pm CDT</span></div></div><p>Lol, I&#039;m sure my fam would he impressed.</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 10:57pm CDT</span></div></div><p>and be hella badazz</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 10:57pm CDT</span></div></div><p>And be like.. I just ran through thunder rain and lightning</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 10:57pm CDT</span></div></div><p>Omg then you can come home and do that hair flip thing</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 10:57pm CDT</span></div></div><p>Yah but isn&#039;t it late?</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Thursday, August 25, 2016 at 10:57pm CDT</span></div></div><p>I might have just heard thunder.</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Thursday, August 25, 2016 at 10:57pm CDT</span></div></div><p>Should I go running?</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Thursday, August 25, 2016 at 10:57pm CDT</span></div></div><p>Lol okay, good.</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 10:53pm CDT</span></div></div><p>I enjoy the Finer things in life... like private ice skating lessons and coldstone... simply because I want to</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 10:53pm CDT</span></div></div><p>No I&#039;d never subscribe to that</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 10:53pm CDT</span></div></div><p>HAHAHAHA, oh jeez.</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Thursday, August 25, 2016 at 10:48pm CDT</span></div></div><p>I suppose you could say it&#039;s for the greater good.</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Thursday, August 25, 2016 at 10:48pm CDT</span></div></div><p>That the privileged are enjoying themselves &quot;for&quot; the people who can&#039;t...</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Thursday, August 25, 2016 at 10:47pm CDT</span></div></div><p>Hmm, it&#039;s an interesting concept, if not.</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 10:45pm CDT</span></div></div><p>I don&#039;t know if I am kidding about that last part</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Thursday, August 25, 2016 at 10:43pm CDT</span></div></div><p>Omg!</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 10:43pm CDT</span></div></div><p>So you have to do it. For them Lucy. It&#039;s up to us</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 10:43pm CDT</span></div></div><p>Not everyone can</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 10:43pm CDT</span></div></div><p>It&#039;s up to us to enjoy the finer things in life</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 10:43pm CDT</span></div></div><p>Oh the great joys of life</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 10:42pm CDT</span></div></div><p>Get a house lady and you&#039;ll be alllll done.</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 10:42pm CDT</span></div></div><p>-sigh- tis so great</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 10:42pm CDT</span></div></div><p>PRIVILEGE WILL LET YOU DO THAT</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 10:42pm CDT</span></div></div><p>DONT WORRY</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Thursday, August 25, 2016 at 10:42pm CDT</span></div></div><p>I am also doing the dishes rn, it would be nice to not have to do that when I grow up...</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Thursday, August 25, 2016 at 10:41pm CDT</span></div></div><p>But this makes me seem way worse than I am. I will get a job next summer and I will back away from relying on my parents.</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Thursday, August 25, 2016 at 10:39pm CDT</span></div></div><p>Parents ❤</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Thursday, August 25, 2016 at 10:38pm CDT</span></div></div><p>It is actually so great to not have a job lol</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Thursday, August 25, 2016 at 10:38pm CDT</span></div></div><p>And of course I continued to lounge around and do pointless shit (not a job)</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Thursday, August 25, 2016 at 10:37pm CDT</span></div></div><p>Umm I am learning to drive. I don&#039;t have my own car cause I&#039;ll be at college but my sister will prob get one for her senior year cause she&#039;s learning too.</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 10:36pm CDT</span></div></div><p>YES! WHAT ELSE?!</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 10:36pm CDT</span></div></div><p>YES!</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Thursday, August 25, 2016 at 10:36pm CDT</span></div></div><p>My dad got me lunch at a restaurant.</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 10:36pm CDT</span></div></div><p>YES!</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Thursday, August 25, 2016 at 10:35pm CDT</span></div></div><p>The housekeeper came today, that was nice.</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 10:34pm CDT</span></div></div><p>Wait, how has your privilege helped you today?</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Thursday, August 25, 2016 at 10:34pm CDT</span></div></div><p>And of course you should also be proud.</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Thursday, August 25, 2016 at 10:34pm CDT</span></div></div><p>Hahaha that&#039;s right!</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 10:32pm CDT</span></div></div><p>Why stop thanking it.</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 10:32pm CDT</span></div></div><p>Oh yeah, the privilege. Hasn&#039;t hurt me yet.</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 10:32pm CDT</span></div></div><p>That nice job after college (or so people tell me.) I don&#039;t appreciate it because I can do better</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 10:31pm CDT</span></div></div><p>God privilege tastes so good.</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 10:31pm CDT</span></div></div><p>The tranquility</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 10:31pm CDT</span></div></div><p>And I get to worry about next months rent but I know that money is coming soon. oh yes</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 10:31pm CDT</span></div></div><p>Oh yeah it&#039;s so good.</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 10:31pm CDT</span></div></div><p>I&#039;m so glad I have my privilege. I get to sit in this apartment in china with my nice AC</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Thursday, August 25, 2016 at 10:31pm CDT</span></div></div><p>But yeah, it will be interesting to see how they implement these things.</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Thursday, August 25, 2016 at 10:30pm CDT</span></div></div><p>Own your privilege Gamal, own it!</p>"""
toB = "Articles in Messages Between Gamal and Lucy on 11/20/2016"