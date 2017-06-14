{-# LANGUAGE NamedFieldPuns #-}

module Import.Mail where

import Network.Mail.Mime
import qualified Network.Mail.Mime.SES as SES
import Import

getMailCreds :: Handler (Maybe (Text, Text, ByteString, ByteString))
getMailCreds = do
    App {appSettings} <- getYesod
    return $ (,,,)
        <$> outgoingAddress appSettings
        <*> sesRegion appSettings
        <*> (encodeUtf8 <$> sesAccess appSettings)
        <*> (encodeUtf8 <$> sesSecret appSettings)

-- | 'sendEmail user subject messageBody'. Forks to the background and logs exceptions
sendEmail :: User -> Text -> Text -> Handler ()
sendEmail user subj msg = forkHandler excs $ do
    creds <- getMailCreds
    manager <- getHttpManager <$> getYesod
    case creds of
        Just (from_, region, access, secret) -> do
            let mail = simpleMail'
                    (Address Nothing $ user ^. userIdent)
                    (Address (Just "Rotothopter") from_)
                    subj
                    (fromStrict msg)
                ses = SES.SES
                    { sesFrom = encodeUtf8 from_
                    , sesTo = [encodeUtf8 $ user ^. userIdent]
                    , sesAccessKey = access
                    , sesSecretKey = secret
                    , sesRegion = region }
            SES.renderSendMailSES manager ses mail
        _ -> $(logWarn) "asked to send email but there are no gmail credentials"
 where
    excs e = $(logWarn) (pack $ show e)
