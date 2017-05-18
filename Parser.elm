module Parser exposing (firstParse
    , secondParse
    , thirdParse
    , parseMeta
    , parseMessages
    , findWord
    , main
    , s, t)

import HtmlParser as HtmlParser exposing (..)
import Regex exposing (..)
import Date exposing (..)
import Chart exposing (..)
import Html exposing (Html)


type alias Message = 
    { user : String
    , meta : String
    , text : String
    }

type alias Meta = 
    { time : String --We can deal with this later
    , zone : String 
    , date : Date.Date
    }

type alias ParsedMessage =
    { user : String
    , meta : Meta
    , text : String
    }

firstParse : String -> List Node 
firstParse s = HtmlParser.parse s 

secondParse : String -> Message 
secondParse s = case HtmlParser.parse s of 
    [Element "div" [("class","message")] ([Element "div" [("class","message_header")] ([Element "span" [("class","user")] ([Text usr]) ,Element "span" [("class","meta")] ([Text mta])])]),Element "p" [] ([Text txt])] -> { user=usr, meta=mta, text=txt }
    _ -> Debug.crash "fail"

thirdParse : List Node -> Maybe ParsedMessage
thirdParse ns = case ns of 
    (Element "div" [("class","message")] ([Element "div" [("class","message_header")] ([Element "span" [("class","user")] ([Text usr]) ,Element "span" [("class","meta")] ([Text mta])])]))::(Element "p" [] ([Text txt]))::rest -> 
        Just { user=usr, meta=parseMeta mta, text=txt }
    _ -> Nothing

parseMessagesFromNodes : List Node -> List ParsedMessage
parseMessagesFromNodes ns = case ns of 
    [] -> []
    [_] -> []
    x::y::rest -> case thirdParse ns of
        Just m -> m::parseMessagesFromNodes rest
        Nothing -> parseMessagesFromNodes (y::rest)

parseMessages : String -> List ParsedMessage
parseMessages s = parseMessagesFromNodes (HtmlParser.parse s)

stringContainsWord : String -> String -> Bool
stringContainsWord s w = 
    let 
        arr = split All (Regex.regex "[,.?! ]") s
    in 
        List.member w arr 

findWord : String -> List ParsedMessage -> List ParsedMessage
findWord w ms = 
    List.filter (\m -> (stringContainsWord m.text w)) ms

parseMeta : String -> Meta
parseMeta m = 
    let 
        arr = split All (Regex.regex ", | ") m
    in 
        case arr of 
            [_,m,d,y,_,t,z] ->
            { time = t
            , zone = z
            , date = getDate (y,m,d)
            }
            _ -> Debug.crash "fail"

getDate : (String, String, String) -> Date 
getDate (y, m, d) = 
    case Date.fromString (y++"-"++(stringToMonthNumber m)++"-"++(appendZeroToNumber d)) of 
        Ok x -> x 
        _ -> Debug.crash "fail"

makeChart : String -> String -> List String -> Html a
makeChart s title xs = 
    let
        ps = parseMessages s
        ys = List.map (\x -> (toFloat (List.length (findWord x ps)), x)) xs
    in 
        Chart.toHtml (Chart.title title (Chart.vBar ys))

main = 
    makeChart t toB ["a", "an", "the"]

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

appendZeroToNumber : String -> String
appendZeroToNumber s = 
    case String.length s of 
        1 -> "0"++s
        2 -> s
        _ -> Debug.crash "fail"

d = "Sunday, November 20, 2016 at 2:16am CST"
s = """<div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Sunday, November 20, 2016 at 2:16am CST</span></div></div><p>Why is this so binary</p>"""
t = """<div class="message"><div class="message_header"><span class="user">Gamal DeWeever</span><span class="meta">Sunday, November 20, 2016 at 2:16am CST</span></div></div><p>Why is this so binary</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Sunday, November 20, 2016 at 2:16am CST</span></div></div><p>Not rly sure what that means</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Sunday, November 20, 2016 at 2:16am CST</span></div></div><p>I did it with what I thought would be your answers and got &quot;next generation left&quot;</p><div class="message"><div class="message_header"><span class="user">100009794951641&#064;facebook.com</span><span class="meta">Sunday, November 20, 2016 at 2:15am CST</span></div></div><p>http://www.people-press.org/quiz/political-typology/</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Sunday, November 20, 2016 at 2:15am CST</span></div></div><p>Sure</p><div class="message"><div class="message_header"><span class="user">1278293023&#064;facebook.com</span><span class="meta">Sunday, November 20, 2016 at 1:03am CST</span></div></div><p>And it&#039;ll show you I&#039;m a libertarian</p><div class="message"><div class="message_header"><span class="user">1278293023&#064;facebook.com</span><span class="meta">Sunday, November 20, 2016 at 1:03am CST</span></div></div><p>It&#039;ll be fun to take</p><div class="message"><div class="message_header"><span class="user">1278293023&#064;facebook.com</span><span class="meta">Sunday, November 20, 2016 at 1:03am CST</span></div></div><p>Why don&#039;t you throw me a test</p><div class="message"><div class="message_header"><span class="user">1278293023&#064;facebook.com</span><span class="meta">Sunday, November 20, 2016 at 1:03am CST</span></div></div><p>3. You said so, and so do the political compasses</p><div class="message"><div class="message_header"><span class="user">1278293023&#064;facebook.com</span><span class="meta">Sunday, November 20, 2016 at 1:03am CST</span></div></div><p>2. Not handouts, premiums for living in this country.</p><div class="message"><div class="message_header"><span class="user">1278293023&#064;facebook.com</span><span class="meta">Sunday, November 20, 2016 at 1:02am CST</span></div></div><p>1. Leftovers</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Sunday, November 20, 2016 at 1:02am CST</span></div></div><p>3) I still don&#039;t understand how you consider yourself a libertarian.</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Sunday, November 20, 2016 at 1:01am CST</span></div></div><p>2) I thought you were advocating for handouts</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Sunday, November 20, 2016 at 1:01am CST</span></div></div><p>1) sharing with a friend is different from handouts</p><div class="message"><div class="message_header"><span class="user">1278293023&#064;facebook.com</span><span class="meta">Sunday, November 20, 2016 at 12:44am CST</span></div></div><p>I DONT NEED YOUR HANDOUTS MAN</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Sunday, November 20, 2016 at 12:40am CST</span></div></div><p>I&#039;m sure there will still be some left.</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Sunday, November 20, 2016 at 12:40am CST</span></div></div><p>You just have to like, tell me next time you come to Chicago</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Sunday, November 20, 2016 at 12:39am CST</span></div></div><p>Sure, theres extra cheesecake</p><div class="message"><div class="message_header"><span class="user">1278293023&#064;facebook.com</span><span class="meta">Sunday, November 20, 2016 at 12:39am CST</span></div></div><p>Sounds great</p><div class="message"><div class="message_header"><span class="user">1278293023&#064;facebook.com</span><span class="meta">Sunday, November 20, 2016 at 12:39am CST</span></div></div><p>Can I have some of that premium no hunger?</p><div class="message"><div class="message_header"><span class="user">Lucy Newman</span><span class="meta">Sunday, November 20, 2016 at 12:39am CST</span></div></div><p>I just ate way too much at the Cheesecake Factory.</p>"""
toB = "Articles in Messages Between Gamal and Lucy on 11/20/2016"
{-
l = [Element "div" [("class","message")] 
([Element "div" [("class","message_header")] 
    ([Element "span" [("class","user")] 
        ([Text "Gamal DeWeever"])
        ,Element "span" [("class","meta")] 
        ([Text "Sunday, November 20, 2016 at 2:16am CST"])])])
,Element "p" [] 
([Text "Why is this so binary"])]
-}