
area_rectangle(10, 5)
area_rectangle(10, -2)
PROGRAM:
area_rectangle <- function(length, breadth) {
 if (length < 0 || breadth < 0) {
 print("Negative input not allowed")
 return(NA)
 }
 return(length * breadth)
}
area_rectangle(10, 5)

area_rectangle(10, -2)
