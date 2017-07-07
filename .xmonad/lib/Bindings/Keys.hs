{-# LANGUAGE FlexibleContexts, FlexibleInstances, NoMonomorphismRestriction #-}
module Bindings.Keys where
import Graphics.X11.ExtraTypes.XF86
import XMonad

import Control.Monad.Writer
import Data.Bits
import Data.Char
import Data.List
import qualified Data.Map as M
import Data.Maybe

-- Additional Keysyms
xF86XK_AudioMicMute         :: KeySym
xF86XK_AudioMicMute         = 269025202

-- Modifiers
mods = "CAWS"
masks = [controlMask, mod1Mask, mod4Mask, shiftMask]

[m_] = chooseMods 0
[mC, mA, mW, mS] = chooseMods 1
[mCA, mCW, mCS, mAW, mAS, mWS] = chooseMods 2
[mCAW, mCAS, mCWS, mAWS] = chooseMods 3
[mCAWS] = chooseMods 4

chooseMods :: Int -> [a -> (KeyMask, a)]
chooseMods = map (,) . map (foldr (.|.) 0) . flip choose masks

choose :: Int -> [a] -> [[a]]
choose n xs = if n > length xs then [] else go n xs
  where
    go 0 _      = [[]]
    go _ []     = []
    go n (x:xs) = map (x:) (go (n-1) xs) ++ choose n xs

-- Key Synonyms
xK :: Char -> KeySym
xK = fromIntegral . ord

xK_Caps  = xK_Caps_Lock
xK_Del   = xK_Delete
xK_Esc   = xK_Escape
xK_Ins   = xK_Insert
xK_PgDn  = xK_Next
xK_PgUp  = xK_Prior
xK_Enter = xK_Return
xK_AMdia = xF86XK_AudioMedia
xK_VolUp = xF86XK_AudioRaiseVolume
xK_VolDn = xF86XK_AudioLowerVolume
xK_Mute  = xF86XK_AudioMute
xK_Fwd   = xF86XK_Forward
xK_Back  = xF86XK_Back
xK_Rfrsh = xF86XK_Reload
xK_Tools = xF86XK_Tools
xK_Think = xF86XK_Launch1
xK_Mic   = xF86XK_AudioMicMute
xK_BriUp = xF86XK_MonBrightnessUp
xK_BriDn = xF86XK_MonBrightnessDown
xK_Power = xF86XK_PowerOff

arrKeys = [xK_Left, xK_Up, xK_Right, xK_Down]
arrStrs = ["←","↑","→","↓"]

numKeys = [xK_1 .. xK_9]
fKeys = [xK_F1 .. xK_F12]

-- Pretty Printing
-- TODO: configerable seperators for lists of bindings

pad :: Int -> String -> String
pad n s = take n s ++ replicate (n - length s) ' '

newtype PrettyMod = PMod {unPMod :: KeyMask}
newtype PrettyMse = PMse {unPMse :: Button}
newtype PrettyKey = PKey {unPKey :: KeySym}
newtype PrettyBind a = PBind {unPBind :: (KeyMask, a)}

instance Show PrettyMod where show = prettyMod . unPMod
instance Show PrettyMse where show = prettyMse . unPMse
instance Show PrettyKey where show = prettyKey . unPKey
instance Show (PrettyBind Button) where
    show = prettyBind (show . PMse) . unPBind
    showList = (++) . prettyBindList
instance Show (PrettyBind KeySym) where
    show = prettyBind (show . PKey) . unPBind
    showList = (++) . prettyKeyBindList

prettyBind p (m, b) = (prettyMod m) ++ " + " ++ p b

prettyBindList = intercalate ", " . map (pad 12 . show)
prettyKeyBindList pbs
  | areKs arrKeys = showPat $ concat arrStrs
  | areKs numKeys = showPat "<N>"
  | areKs fKeys   = showPat "F<N>"
  | otherwise = prettyBindList pbs
  where
    bs = map unPBind pbs
    m = fst . head $ bs
    areKs ks = all (== m) (map fst bs) && map snd bs == ks
    showPat s = prettyMod m ++ " + " ++ s

prettyMod m = flip map mods $ \c -> if maskOf c .&. m == 0 then '_' else c
  where maskOf = fromJust . flip lookup (zip mods masks)

prettyMse = ("B" ++) . show

prettyKey key = format
  . fromMaybe (keysymToString key)
  . M.lookup key
  $ execWriter $ do
    xK_Caps_Lock                # "Caps"
    xK_Delete                   # "Del"
    xK_Escape                   # "Esc"
    xK_Insert                   # "Ins"
    xK_Prior                    # "Prev"
    xK_Return                   # "Enter"
    xF86XK_AudioRaiseVolume     # "VolUp"
    xF86XK_AudioLowerVolume     # "VolDn"
    xF86XK_AudioMute            # "Mute"
    xF86XK_Forward              # "Fwd"
    xF86XK_Back                 # "Back"
    xF86XK_Reload               # "Rfrsh"
    xF86XK_Launch1              # "Think"
    xF86XK_Launch2              # "Mic"
    xF86XK_MonBrightnessUp      # "BriUp"
    xF86XK_MonBrightnessDown    # "BriDn"
    xK_KP_Add                   # "KP+"
    xK_KP_Subtract              # "KP-"
    xK_KP_Multiply              # "KP*"
    xK_KP_Divide                # "KP/"
    xK ' '                      # "' '"
    tellZ arrKeys arrStrs
    let cs = [33..126] in tellZM cs $ return . chr . fromIntegral
    let cs = fKeys     in tellZM cs $ \c -> "F" ++ show (c - xK_F1 + 1)
  where
    k # s = tell $ M.singleton k s
    tellZ  ks vs = tell . M.fromList $ zip ks vs
    tellZM ks f  = tellZ ks $ map f ks

    format str = take 5 . (++ repeat ' ') . filter (/= '_')
               $ if "XF86" `isPrefixOf` str then drop 4 str else str

