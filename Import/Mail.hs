{-# LANGUAGE NamedFieldPuns #-}

module Import.Mail where

import Network.Mail.Client.Gmail
import Network.Mail.Mime
import Import

getGmailCreds :: Handler (Maybe (Text, Text))
getGmailCreds = do
    App {appSettings} <- getYesod
    return $ liftA2 (,) (gmailAddress appSettings) (gmailPassword appSettings)

-- | 'sendEmail user subject messageBody'. Forks to the background and logs exceptions
sendEmail :: User -> Text -> Text -> Handler ()
sendEmail user subj msg = forkHandler excs $ do
    creds <- getGmailCreds
    case creds of
        Just (from_, password) -> 
            liftIO $ sendGmail (fromStrict from_) (fromStrict password)
                (Address (Just "Rotothopter") from_) -- from
                [Address Nothing $ user ^. userIdent] -- to
                [] [] -- cc and bcc
                subj
                (fromStrict msg)
                [] -- file attachments
                10000000 -- timeout in ms
        _ -> $(logWarn) "asked to send email but there are no gmail credentials"
 where
    excs e = $(logWarn) (pack $ show e)
