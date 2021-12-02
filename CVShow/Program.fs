open SharpCV
let cv2 = SharpCV.Binding.cv2

// For more information see https://aka.ms/fsharp-console-apps
printfn "Hello from F#"

let img = cv2.imread("../../../../../18_2703.jpg")
cv2.imshow("Manga", img)

cv2.waitKey(0)