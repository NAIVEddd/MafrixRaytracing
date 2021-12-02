open SharpCV
open Engine.Model.Obj

let cv2 = SharpCV.Binding.cv2

// For more information see https://aka.ms/fsharp-console-apps
LoadModel("C:\\allFiles\\Game\\LittleGraphicShow\\.vs\\Assets\\Renault12TL\\Renault12TL.obj")

let img = cv2.imread("../../../../../18_2703.jpg")
cv2.imshow("Manga", img)

cv2.waitKey(0)