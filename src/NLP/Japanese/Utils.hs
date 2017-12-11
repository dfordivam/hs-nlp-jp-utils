-- | Some utility functions related for Japanese text processing
module NLP.Japanese.Utils
  (
    -- * Only applies to jp kana
    katakanaToHiragana
  , hiraganaToKatakana
    -- * Get Kanji Characters
    --   The Kanji detection is not limited to Japanese Characters
    --   It just considers the whole unicode CJK set.
  , getKanjis
  , hasKanaOrKanji
  , isKana
  , isKanji)
  where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char

-- Utility Functions

katakanaToHiragana :: Text -> Text
katakanaToHiragana = T.map getH

hiraganaToKatakana :: Text -> Text
hiraganaToKatakana = T.map getK

-- Hiragana ( 3040 - 309f)
-- Katakana ( 30a0 - 30ff)
--  Full-width roman characters and half-width katakana ( ff00 - ffef)
--   CJK unifed ideographs - Common and uncommon kanji ( 4e00 - 9faf)
--   CJK unified ideographs Extension A - Rare kanji ( 3400 - 4dbf)

-- ^  Filter valid Kanji (no hiragana or katakana)
getKanjis :: Text -> [Text]
getKanjis inp = map T.pack $ map (:[]) $ filter isKanji $ T.unpack inp

hasKanaOrKanji :: Text -> Bool
hasKanaOrKanji = (all f) . T.unpack
  where f c = isKana c || isKanji c

-- 3040 - 30ff
isKana c = c > l && c < h
  where l = chr $ 12352
        h = chr $ 12543

-- 3400 - 9faf
isKanji c = c > l && c < h
 where l = chr $ 13312
       h = chr $ 40879

getH 'ア' = 'あ'
getH 'ィ' = 'ぃ'
getH 'イ' = 'い'
getH 'ゥ' = 'ぅ'
getH 'ウ' = 'う'
getH 'ェ' = 'ぇ'
getH 'エ' = 'え'
getH 'ォ' = 'ぉ'
getH 'オ' = 'お'
getH 'カ' = 'か'
getH 'ガ' = 'が'
getH 'キ' = 'き'
getH 'ギ' = 'ぎ'
getH 'ク' = 'く'
getH 'グ' = 'ぐ'
getH 'ケ' = 'け'
getH 'ゲ' = 'げ'
getH 'コ' = 'こ'
getH 'ゴ' = 'ご'
getH 'サ' = 'さ'
getH 'ザ' = 'ざ'
getH 'シ' = 'し'
getH 'ジ' = 'じ'
getH 'ス' = 'す'
getH 'ズ' = 'ず'
getH 'セ' = 'せ'
getH 'ゼ' = 'ぜ'
getH 'ソ' = 'そ'
getH 'ゾ' = 'ぞ'
getH 'タ' = 'た'
getH 'ダ' = 'だ'
getH 'チ' = 'ち'
getH 'ヂ' = 'ぢ'
getH 'ッ' = 'っ'
getH 'ツ' = 'つ'
getH 'ヅ' = 'づ'
getH 'テ' = 'て'
getH 'デ' = 'で'
getH 'ト' = 'と'
getH 'ド' = 'ど'
getH 'ナ' = 'な'
getH 'ニ' = 'に'
getH 'ヌ' = 'ぬ'
getH 'ネ' = 'ね'
getH 'ノ' = 'の'
getH 'ハ' = 'は'
getH 'バ' = 'ば'
getH 'パ' = 'ぱ'
getH 'ヒ' = 'ひ'
getH 'ビ' = 'び'
getH 'ピ' = 'ぴ'
getH 'フ' = 'ふ'
getH 'ブ' = 'ぶ'
getH 'プ' = 'ぷ'
getH 'ヘ' = 'へ'
getH 'ベ' = 'べ'
getH 'ペ' = 'ぺ'
getH 'ホ' = 'ほ'
getH 'ボ' = 'ぼ'
getH 'ポ' = 'ぽ'
getH 'マ' = 'ま'
getH 'ミ' = 'み'
getH 'ム' = 'む'
getH 'メ' = 'め'
getH 'モ' = 'も'
getH 'ャ' = 'ゃ'
getH 'ヤ' = 'や'
getH 'ュ' = 'ゅ'
getH 'ユ' = 'ゆ'
getH 'ョ' = 'ょ'
getH 'ヨ' = 'よ'
getH 'ラ' = 'ら'
getH 'リ' = 'り'
getH 'ル' = 'る'
getH 'レ' = 'れ'
getH 'ロ' = 'ろ'
getH 'ヮ' = 'ゎ'
getH 'ワ' = 'わ'
getH 'ヲ' = 'を'
getH 'ン' = 'ん'
getH c = c

getK 'あ' = 'ア'
getK 'ぃ' = 'ィ'
getK 'い' = 'イ'
getK 'ぅ' = 'ゥ'
getK 'う' = 'ウ'
getK 'ぇ' = 'ェ'
getK 'え' = 'エ'
getK 'ぉ' = 'ォ'
getK 'お' = 'オ'
getK 'か' = 'カ'
getK 'が' = 'ガ'
getK 'き' = 'キ'
getK 'ぎ' = 'ギ'
getK 'く' = 'ク'
getK 'ぐ' = 'グ'
getK 'け' = 'ケ'
getK 'げ' = 'ゲ'
getK 'こ' = 'コ'
getK 'ご' = 'ゴ'
getK 'さ' = 'サ'
getK 'ざ' = 'ザ'
getK 'し' = 'シ'
getK 'じ' = 'ジ'
getK 'す' = 'ス'
getK 'ず' = 'ズ'
getK 'せ' = 'セ'
getK 'ぜ' = 'ゼ'
getK 'そ' = 'ソ'
getK 'ぞ' = 'ゾ'
getK 'た' = 'タ'
getK 'だ' = 'ダ'
getK 'ち' = 'チ'
getK 'ぢ' = 'ヂ'
getK 'っ' = 'ッ'
getK 'つ' = 'ツ'
getK 'づ' = 'ヅ'
getK 'て' = 'テ'
getK 'で' = 'デ'
getK 'と' = 'ト'
getK 'ど' = 'ド'
getK 'な' = 'ナ'
getK 'に' = 'ニ'
getK 'ぬ' = 'ヌ'
getK 'ね' = 'ネ'
getK 'の' = 'ノ'
getK 'は' = 'ハ'
getK 'ば' = 'バ'
getK 'ぱ' = 'パ'
getK 'ひ' = 'ヒ'
getK 'び' = 'ビ'
getK 'ぴ' = 'ピ'
getK 'ふ' = 'フ'
getK 'ぶ' = 'ブ'
getK 'ぷ' = 'プ'
getK 'へ' = 'ヘ'
getK 'べ' = 'ベ'
getK 'ぺ' = 'ペ'
getK 'ほ' = 'ホ'
getK 'ぼ' = 'ボ'
getK 'ぽ' = 'ポ'
getK 'ま' = 'マ'
getK 'み' = 'ミ'
getK 'む' = 'ム'
getK 'め' = 'メ'
getK 'も' = 'モ'
getK 'ゃ' = 'ャ'
getK 'や' = 'ヤ'
getK 'ゅ' = 'ュ'
getK 'ゆ' = 'ユ'
getK 'ょ' = 'ョ'
getK 'よ' = 'ヨ'
getK 'ら' = 'ラ'
getK 'り' = 'リ'
getK 'る' = 'ル'
getK 'れ' = 'レ'
getK 'ろ' = 'ロ'
getK 'ゎ' = 'ヮ'
getK 'わ' = 'ワ'
getK 'を' = 'ヲ'
getK 'ん' = 'ン'
getK c = c
