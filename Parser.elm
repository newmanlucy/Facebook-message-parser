module Parser exposing (thirdParse
    , parseMeta
    , parseMessages
    , findWord
    , getTime 
    , getDate
    , histogram
    , gamalUsrRegex
    , stringWordCount
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
                Just n -> insert word (n+1) dict
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
                    _ -> Debug.crash "Fail"
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
        [] -> Debug.crash "findIndex: Not found (shouldn't happen)"
        x_::xs_ -> if x == x_ then 0 else 1 + (findIndex x xs_)

findElt : Int -> List a -> a
findElt n xs = 
    case xs of 
        [] -> Debug.crash "findElt: Oob (shouldn't happen)"
        x::xs_ -> if n == 0 then x else findElt (n-1) xs_

getWords : ParsedMessage -> List String
getWords pm = 
    let words = pm.text in
    split All (Regex.regex "[,.?! ]") words

----------------------------------------------------------------------------
{-making graph with the graphics library-}
----------------------------------------------------------------------------

{-makeBarInfos helper function-}
makeBarInfos_ : List ParsedMessage -> String -> Dict BarLabel Freq -> Dict BarLabel Freq
makeBarInfos_ msgs s d = case msgs of 
    [] -> d
    hd::rest -> 
        let 
            date = hd.meta.date
            e = (Date.year date, monthToInt (Date.month date), s, hd.user)
            n = stringWordCount hd.text s
        in 
            if (Dict.member e d) then
                (Dict.update e (updateFreq n) d)
            else Dict.insert e n d

{-given list of parsed messages, return a dictionary of (BarLabel, Freq)
 -with frequencies of each word for each user and (month,year) combination-}
makeBarInfos : List ParsedMessage -> String -> Dict BarLabel Freq
makeBarInfos msgs s = makeBarInfos_ msgs s Dict.empty

{- make one rectangle with color depending on user and string -}
makeRectangle : Float -> Regex -> Regex -> List String -> BarLabel -> Freq -> Float -> Form
makeRectangle width usr1 usr2 strs lbl fq scl = 
    case lbl of 
        (y,m,w,u) -> 
            let 
                shade = if Regex.contains usr1 u then darkColors 
                else if Regex.contains usr2 u then lightColors
                else medColors
                index = (findIndex w strs) % 7 -- number of colors per shade
                color = findElt index shade
                rect = Collage.filled color (Collage.rect  width ((toFloat fq) * scl + (scl/2)))
                text = Collage.text (Text.fromString (u ++ "." ++ w ++ ":" ++ (toString fq)))
                {-newline not working?-}
                obj = Collage.group [rect, (Collage.moveY (-1 * width) text)]
            in
                obj

{-moveRectangles helper-}        
moveRectangles_ : Int -> Float -> List Form -> List Form
moveRectangles_ n width rs = 
    case rs of 
        [] -> []
        r::rs_ -> (Collage.moveX ((toFloat n) * width) r) 
                  :: (moveRectangles_ (n+1) width rs_)

{-move rectangles to be spaced next to each other // needs work-}
moveRectangles : Float -> List Form -> List Form
moveRectangles width rs = moveRectangles_ 0 width rs

{-make rectangles for a given string an list of parsed messages-}
makeRectangles : Float -> Float -> Regex -> Regex -> List String -> List ParsedMessage -> List Form
makeRectangles width scl usr1 usr2 strs ms = 
    let 
        ds = List.map (makeBarInfos ms) strs
        ls = List.map Dict.toList ds 
        l1 = List.foldr (++) [] ls
        l2 = List.sortWith orderBarLabel l1
        {-add in empties?-}
        l4 = List.map 
            (\x -> makeRectangle width usr1 usr2 strs (Tuple.first x) (Tuple.second x) scl) 
            l2
        l5 = moveRectangles (width * 1.2) l4
        {-make labels-}
    in
        l5

{-make rectangles into collage-}
makeCollage : Float -> List Form -> Element.Element
makeCollage width fs = 
    let 
        length = ((floor width) * (List.length fs)) * 3
    in 
        Collage.collage length length fs 

---------------------------------------------------------------------------------
{-model, view, update-}
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

initialModel : Model
initialModel = {words=[], file=Nothing, nextWord=""}

update : Msg -> Model -> Model 
update msg model = 
    case msg of 
        Word w -> {model | nextWord = w}
        File f -> {model | file = Just f}
        ResetWords -> {model | words = []}
        Reset -> initialModel
        Enter -> if String.isEmpty model.nextWord then model 
            else {model | words = model.nextWord::model.words, nextWord = ""}

view : Model -> Html Msg 
view model = 
    let 
        resetWords = Html.button [onClick ResetWords] [Html.text "Reset Words"] 
        reset = Html.button [onClick Reset] [Html.text "Reset"]
        enter = Html.button [onClick Enter] [Html.text "Enter"]
        word = Html.input [ type_ "word", placeholder "Enter word", onInput Word ] []
        file = Html.input [type_ "file", placeholder "Enter file", onInput File ] []
        display = Html.text ("Words: " ++ (toString model.words))
        enterword = Html.span [] [word, enter]
        resets = Html.span [] [resetWords, reset]
        break = Html.br [] []
        ms = parseMessages u
        w = 100
        rs = makeRectangles w w lucyUsrRegex gamalUsrRegex model.words ms
        c = makeCollage w rs
        collage = Html.div [] [Element.toHtml c] 
        all =
            Html.div 
                [] 
                [ enterword
                , resets
                , file
                , break
                , display
                , break
                , collage ]
    in 
        all
        --Html.span [] [word, enter, file, fileenter, resetWords, reset, display]

main : Program Never Model Msg
main = Html.beginnerProgram 
    { model = initialModel
    , view = view
    , update = update
    }

---------------------------------------------------------------------------------
{-main-}
---------------------------------------------------------------------------------
main2 : Html msg
main2 = 
    let 
        ms = parseMessages u
        w = 100
        rs = makeRectangles w w lucyUsrRegex gamalUsrRegex ["to"] ms
        c = makeCollage w rs 
        style =
        Attr.style <|
          [ ("position", "fixed")
          , ("top", "50%")
          , ("left", "50%")
          , ("transform", "translate(-50%, -50%)")
          ]
        text = Html.text ("This is like a graph or something? " ++ (toString (List.length rs)))
    in 
        Html.div [style] [text, Element.toHtml c]

----------------------------------------------------------------------------------
{-make charts sing other libraries-}
----------------------------------------------------------------------------------
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
                (\x -> ((4 < String.length (Tuple.second x)) && (2 < Tuple.first x)) || (8 < Tuple.first x)) 
                (List.map 
                        (\x -> (toFloat (Tuple.second x), Tuple.first x))
                        (Dict.toList (Dict.remove "" (histogram s)))))
    in 
        Chart.toHtml (Chart.title title (Chart.vBar d))

{-
main = 
    graphHistogram u "histogram of some of the words in a sample of Facebook messages"
-}

--------------------------------------------------------------------------------
{-strings, etc. for testing-}
--------------------------------------------------------------------------------
gamalUsrRegex = Regex.regex "Gamal DeWeever|1278293023@facebook.com"
lucyUsrRegex = Regex.regex "Lucy Newman|100009794951641@facebook.com"
-- for some reason \. is causing problems? . matches any single char

d = "Sunday, November 20, 2016 at 2:16am CST"
s = """<div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Sunday, November 20, 2016 at 2:16am CST</span></div></div><p>Why is this so binary</p>"""
t = """<div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Sunday, November 20, 2016 at 2:16am CST</span></div></div><p>Why is this so binary</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Sunday, November 20, 2016 at 2:16am CST</span></div></div><p>Not rly sure what that means</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Sunday, November 20, 2016 at 2:16am CST</span></div></div><p>I did it with what I thought would be your answers and got &quot;next generation left&quot;</p><div class="message"><div class="message_header"><span class="user">100009794951641&#064;facebook.com</span><span class="meta">Sunday, November 20, 2016 at 2:15am CST</span></div></div><p>http://www.people-press.org/quiz/political-typology/</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Sunday, November 20, 2016 at 2:15am CST</span></div></div><p>Sure</p><div class="message"><div class="message_header"><span class="user">1278293023&#064;facebook.com</span><span class="meta">Sunday, November 20, 2016 at 1:03am CST</span></div></div><p>And it&#039;ll show you I&#039;m a libertarian</p><div class="message"><div class="message_header"><span class="user">1278293023&#064;facebook.com</span><span class="meta">Sunday, November 20, 2016 at 1:03am CST</span></div></div><p>It&#039;ll be fun to take</p><div class="message"><div class="message_header"><span class="user">1278293023&#064;facebook.com</span><span class="meta">Sunday, November 20, 2016 at 1:03am CST</span></div></div><p>Why don&#039;t you throw me a test</p><div class="message"><div class="message_header"><span class="user">1278293023&#064;facebook.com</span><span class="meta">Sunday, November 20, 2016 at 1:03am CST</span></div></div><p>3. You said so, and so do the political compasses</p><div class="message"><div class="message_header"><span class="user">1278293023&#064;facebook.com</span><span class="meta">Sunday, November 20, 2016 at 1:03am CST</span></div></div><p>2. Not handouts, premiums for living in this country.</p><div class="message"><div class="message_header"><span class="user">1278293023&#064;facebook.com</span><span class="meta">Sunday, November 20, 2016 at 1:02am CST</span></div></div><p>1. Leftovers</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Sunday, November 20, 2016 at 1:02am CST</span></div></div><p>3) I still don&#039;t understand how you consider yourself a libertarian.</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Sunday, November 20, 2016 at 1:01am CST</span></div></div><p>2) I thought you were advocating for handouts</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Sunday, November 20, 2016 at 1:01am CST</span></div></div><p>1) sharing with a friend is different from handouts</p><div class="message"><div class="message_header"><span class="user">1278293023&#064;facebook.com</span><span class="meta">Sunday, November 20, 2016 at 12:44am CST</span></div></div><p>I DONT NEED YOUR HANDOUTS MAN</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Sunday, November 20, 2016 at 12:40am CST</span></div></div><p>I&#039;m sure there will still be some left.</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Sunday, November 20, 2016 at 12:40am CST</span></div></div><p>You just have to like, tell me next time you come to Chicago</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Sunday, November 20, 2016 at 12:39am CST</span></div></div><p>Sure, theres extra cheesecake</p><div class="message"><div class="message_header"><span class="user">1278293023&#064;facebook.com</span><span class="meta">Sunday, November 20, 2016 at 12:39am CST</span></div></div><p>Sounds great</p><div class="message"><div class="message_header"><span class="user">1278293023&#064;facebook.com</span><span class="meta">Sunday, November 20, 2016 at 12:39am CST</span></div></div><p>Can I have some of that premium no hunger?</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Sunday, November 20, 2016 at 12:39am CST</span></div></div><p>I just ate way too much at the Cheesecake Factory.</p>"""
u = """<div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Friday, August 26, 2016 at 11:28am CDT</span></div></div><p>I am having my blood donated and the gave me to a penguin to keep me company to to to.</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Friday, August 26, 2016 at 11:27am CDT</span></div></div><p>I was just curious to cause like you&#039;re the first to year of booth.</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Friday, August 26, 2016 at 11:24am CDT</span></div></div><p>Were they ppl from ihouse to or like just random ppl or what??</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Friday, August 26, 2016 at 11:24am CDT</span></div></div><p>Yeah, I guess.</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Friday, August 26, 2016 at 11:23am CDT</span></div></div><p>Wow.</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Friday, August 26, 2016 at 11:23am CDT</span></div></div><p>that was... nearly four years ago to </p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Friday, August 26, 2016 at 11:23am CDT</span></div></div><p>I just don&#039;t remember them</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Friday, August 26, 2016 at 11:23am CDT</span></div></div><p>Lol, I did</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Friday, August 26, 2016 at 11:21am CDT</span></div></div><p>Did you not go to oweek??</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Friday, August 26, 2016 at 11:21am CDT</span></div></div><p>???</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Friday, August 26, 2016 at 11:17am CDT</span></div></div><p>Lol, no idea</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Friday, August 26, 2016 at 12:35am CDT</span></div></div><p>Who were the oaides your year???</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Thursday, August 25, 2016 at 11:02pm CDT</span></div></div><p>🏄</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 11:02pm CDT</span></div></div><p>Surfs up dude</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 11:01pm CDT</span></div></div><p>󾟚</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 11:01pm CDT</span></div></div><p>Remember to exercise your privilege if you can</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 11:01pm CDT</span></div></div><p>Okay you go running. I&#039;ll take a nap</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Thursday, August 25, 2016 at 11:01pm CDT</span></div></div><p>Hopefully no one thinks of that idea before I publish it.</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Thursday, August 25, 2016 at 11:01pm CDT</span></div></div><p>Exactly.</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 11:00pm CDT</span></div></div><p>And then you can innovate</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Thursday, August 25, 2016 at 11:00pm CDT</span></div></div><p>I&#039;ll bring my kite with a key attached to it.</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Thursday, August 25, 2016 at 11:00pm CDT</span></div></div><p>Okay, sounds like a plan.</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Thursday, August 25, 2016 at 11:00pm CDT</span></div></div><p>Well in the future I will know whether it rains tonight or not so I won&#039;t have to say possibly.</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 10:59pm CDT</span></div></div><p>Like if you run today and you don&#039;t want to run in the future, you can say.. remember that one day you ran and it was hot and possibly raining?</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Thursday, August 25, 2016 at 10:59pm CDT</span></div></div><p>And possibly raining.</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Thursday, August 25, 2016 at 10:58pm CDT</span></div></div><p>But like, it&#039;s hot out.</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 10:58pm CDT</span></div></div><p>Oh then yeah probs</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Thursday, August 25, 2016 at 10:58pm CDT</span></div></div><p>Yeah, I usually run this late.</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Thursday, August 25, 2016 at 10:58pm CDT</span></div></div><p>Lol, I&#039;m sure my fam would he impressed.</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 10:57pm CDT</span></div></div><p>and be hella badazz</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 10:57pm CDT</span></div></div><p>And be like.. I just ran through thunder rain and lightning</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 10:57pm CDT</span></div></div><p>Omg then you can come home and do that hair flip thing</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 10:57pm CDT</span></div></div><p>Yah but isn&#039;t it late?</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Thursday, August 25, 2016 at 10:57pm CDT</span></div></div><p>I might have just heard thunder.</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Thursday, August 25, 2016 at 10:57pm CDT</span></div></div><p>Should I go running?</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Thursday, August 25, 2016 at 10:57pm CDT</span></div></div><p>Lol okay, good.</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 10:53pm CDT</span></div></div><p>I enjoy the Finer things in life... like private ice skating lessons and coldstone... simply because I want to</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 10:53pm CDT</span></div></div><p>No I&#039;d never subscribe to that</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 10:53pm CDT</span></div></div><p>HAHAHAHA, oh jeez.</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Thursday, August 25, 2016 at 10:48pm CDT</span></div></div><p>I suppose you could say it&#039;s for the greater good.</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Thursday, August 25, 2016 at 10:48pm CDT</span></div></div><p>That the privileged are enjoying themselves &quot;for&quot; the people who can&#039;t...</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Thursday, August 25, 2016 at 10:47pm CDT</span></div></div><p>Hmm, it&#039;s an interesting concept, if not.</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 10:45pm CDT</span></div></div><p>I don&#039;t know if I am kidding about that last part</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Thursday, August 25, 2016 at 10:43pm CDT</span></div></div><p>Omg!</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 10:43pm CDT</span></div></div><p>So you have to do it. For them Lucy. It&#039;s up to us</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 10:43pm CDT</span></div></div><p>Not everyone can</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 10:43pm CDT</span></div></div><p>It&#039;s up to us to enjoy the finer things in life</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 10:43pm CDT</span></div></div><p>Oh the great joys of life</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 10:42pm CDT</span></div></div><p>Get a house lady and you&#039;ll be alllll done.</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 10:42pm CDT</span></div></div><p>-sigh- tis so great</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 10:42pm CDT</span></div></div><p>PRIVILEGE WILL LET YOU DO THAT</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 10:42pm CDT</span></div></div><p>DONT WORRY</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Thursday, August 25, 2016 at 10:42pm CDT</span></div></div><p>I am also doing the dishes rn, it would be nice to not have to do that when I grow up...</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Thursday, August 25, 2016 at 10:41pm CDT</span></div></div><p>But this makes me seem way worse than I am. I will get a job next summer and I will back away from relying on my parents.</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Thursday, August 25, 2016 at 10:39pm CDT</span></div></div><p>Parents ❤</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Thursday, August 25, 2016 at 10:38pm CDT</span></div></div><p>It is actually so great to not have a job lol</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Thursday, August 25, 2016 at 10:38pm CDT</span></div></div><p>And of course I continued to lounge around and do pointless shit (not a job)</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Thursday, August 25, 2016 at 10:37pm CDT</span></div></div><p>Umm I am learning to drive. I don&#039;t have my own car cause I&#039;ll be at college but my sister will prob get one for her senior year cause she&#039;s learning too.</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 10:36pm CDT</span></div></div><p>YES! WHAT ELSE?!</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 10:36pm CDT</span></div></div><p>YES!</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Thursday, August 25, 2016 at 10:36pm CDT</span></div></div><p>My dad got me lunch at a restaurant.</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 10:36pm CDT</span></div></div><p>YES!</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Thursday, August 25, 2016 at 10:35pm CDT</span></div></div><p>The housekeeper came today, that was nice.</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 10:34pm CDT</span></div></div><p>Wait, how has your privilege helped you today?</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Thursday, August 25, 2016 at 10:34pm CDT</span></div></div><p>And of course you should also be proud.</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Thursday, August 25, 2016 at 10:34pm CDT</span></div></div><p>Hahaha that&#039;s right!</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 10:32pm CDT</span></div></div><p>Why stop thanking it.</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 10:32pm CDT</span></div></div><p>Oh yeah, the privilege. Hasn&#039;t hurt me yet.</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 10:32pm CDT</span></div></div><p>That nice job after college (or so people tell me.) I don&#039;t appreciate it because I can do better</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 10:31pm CDT</span></div></div><p>God privilege tastes so good.</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 10:31pm CDT</span></div></div><p>The tranquility</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 10:31pm CDT</span></div></div><p>And I get to worry about next months rent but I know that money is coming soon. oh yes</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 10:31pm CDT</span></div></div><p>Oh yeah it&#039;s so good.</p><div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Thursday, August 25, 2016 at 10:31pm CDT</span></div></div><p>I&#039;m so glad I have my privilege. I get to sit in this apartment in china with my nice AC</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Thursday, August 25, 2016 at 10:31pm CDT</span></div></div><p>But yeah, it will be interesting to see how they implement these things.</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Thursday, August 25, 2016 at 10:30pm CDT</span></div></div><p>Own your privilege Gamal, own it!</p>"""
toB = "Articles in Messages Between Gamal and Lucy on 11/20/2016"