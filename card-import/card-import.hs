import Application (handler)
import Handler.UpdateMtgJson (postUpdateMtgJsonR)

main = handler $ postUpdateMtgJsonR
