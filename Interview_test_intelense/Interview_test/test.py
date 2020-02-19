import cv2
#import os module for reading training data directories and paths
import os
#import numpy to convert python lists to numpy arrays as 
#it is needed by OpenCV face recognizers
import numpy as np
from sklearn import preprocessing

test = [ i for i in os.listdir('data') if '.' not in i ]
faceCascade = cv2.CascadeClassifier(cv2.data.haarcascades + "haarcascade_frontalface_default.xml")

train_data = []
for folder_name in test:
    for file_name in os.listdir('data/'+folder_name):
        image = cv2.imread('data/'+folder_name+'/'+file_name)
        gray = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)
        faces = faceCascade.detectMultiScale(
            gray,
            scaleFactor=1.1,
            minNeighbors=5,
            minSize=(30, 30),
            flags=cv2.CASCADE_SCALE_IMAGE
        )
        if (len(faces) == 1):
            (x, y, w, h) = faces[0]
            train_data.append((gray[y:y+w, x:x+h], folder_name))

# cv2.imshow('image', train_data[20][0])
# cv2.waitKey(0)
# cv2.destroyAllWindows()

face_recognizer = cv2.face.LBPHFaceRecognizer_create()
train_data = np.array(train_data)
le = preprocessing.LabelEncoder()
#print(train_data[:, 1])
le.fit(train_data[:, 1])
face_recognizer.train(train_data[:, 0], le.transform(train_data[:, 1]))

video_capture = cv2.VideoCapture(0)
while True:
    # Capture frame-by-frame
    ret, frame = video_capture.read()

    gray = cv2.cvtColor(frame, cv2.COLOR_BGR2GRAY)

    faces = faceCascade.detectMultiScale(
        gray,
        scaleFactor=1.1,
        minNeighbors=5,
        minSize=(30, 30),
        flags=cv2.CASCADE_SCALE_IMAGE
    )

    #predict the image using our face recognizer 

    # Draw a rectangle around the faces
    for (x, y, w, h) in faces:
        cv2.rectangle(frame, (x, y), (x+w, y+h), (0, 255, 0), 2)
        verify = gray[y:y+w, x:x+h]
        label, confidence = face_recognizer.predict(verify)
        print(le.inverse_transform([int(label)]))
        cv2.putText(frame, str(le.inverse_transform([int(label)])[0]), (x, y), cv2.FONT_HERSHEY_COMPLEX, 1.5, (0, 255, 0), 2)
    # Display the resulting frame
    cv2.imshow('Video', frame)

    if cv2.waitKey(1) & 0xFF == ord('q'):
        break

# When everything is done, release the capture
video_capture.release()
cv2.destroyAllWindows()