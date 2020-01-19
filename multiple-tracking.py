# code adapted from https://www.pyimagesearch.com/2018/08/06/tracking-multiple-objects-with-opencv/
# import the necessary packages
from imutils.video import VideoStream
import argparse
import imutils
import time
import cv2
import numpy as np

# construct the argument parser and parse the arguments
ap = argparse.ArgumentParser()
ap.add_argument("-v", "--video", type = str, help = "path to input video file")
ap.add_argument("-t", "--tracker", type = str, default = "kcf", help = "OpenCV object tracker type")
ap.add_argument("-f", "--filename", type = str, default = "output.csv", help = "output log file name")
ap.add_argument("-l", "--log", type = int, default = 5000, help = "interval for recording object coordinates in milliseconds")
args = vars(ap.parse_args())

# initialize a dictionary that maps strings to their corresponding OpenCV object tracker implementations
OPENCV_OBJECT_TRACKERS = {
	"csrt": cv2.TrackerCSRT_create,
	"kcf": cv2.TrackerKCF_create,
	"boosting": cv2.TrackerBoosting_create,
	"mil": cv2.TrackerMIL_create,
	"tld": cv2.TrackerTLD_create,
	"medianflow": cv2.TrackerMedianFlow_create,
	"mosse": cv2.TrackerMOSSE_create
}

# initialize OpenCV's special multi-object tracker
# file = open(filename, 'w')
trackers = cv2.MultiTracker_create()
IDs = []

# if a video path was not supplied, grab the reference to the web cam
if not args.get("video", False):
	print("[INFO] starting video stream...")
	vs = VideoStream(src = 0).start()
	time.sleep(1.0)

# otherwise, grab a reference to the video file
else:
	vs = cv2.VideoCapture(args["video"])

# loop over frames from the video stream
while True:
	# grab the current frame, then handle if we are using a VideoStream or VideoCapture object
	frame = vs.read()
	frame = frame[1] if args.get("video", False) else frame

	# check to see if we have reached the end of the stream
	if frame is None:
		break

	# resize the frame (so we can process it faster)
	frame = imutils.resize(frame, width = 600)
	timestamp = vs.get(cv2.CAP_PROP_POS_MSEC)

	# grab the updated bounding box coordinates (if any) for each object that is being tracked
	(success, boxes) = trackers.update(frame)

	# loop over the bounding boxes and draw then on the frame
	for index, box in enumerate(boxes):
		(x, y, w, h) = [int(v) for v in box]
		cv2.rectangle(frame, (x, y), (x + w, y + h), (0, 255, 0), 2)
		cv2.putText(frame, IDs[index], (x, y), cv2.FONT_HERSHEY_SIMPLEX, 0.5, (255, 255, 255), 2)

	# show the output frame
	cv2.imshow("Frame", frame)
	key = cv2.waitKey(1) & 0xFF

	# if the 's' key is selected, we are going to "select" a bounding box to track
	if key == ord("s"):
		while True:
			try:
				# select the bounding box of the object we want to track (make sure you press ENTER or SPACE after selecting the ROI)
				box = cv2.selectROI("Frame", frame, fromCenter = False, showCrosshair = True)
				box_id = input('Object ID:')
				IDs.append(box_id)

				# create a new object tracker for the bounding box and add it to our multi-object tracker
				tracker = OPENCV_OBJECT_TRACKERS[args["tracker"]]()
				trackers.add(tracker, frame, box)

				add_object = input('Select another object? [y/n]')
				if add_object == 'n':
					break
			except:
				break

	elif key == ord("r"):
		while True:
			try:
				box_id = input('Object to remove:')
				box_index = IDs.index(box_id)
				IDs.remove(box_id)
				boxes = np.delete(trackers.getObjects().copy(), box_index, 0)

				trackers = cv2.MultiTracker_create()
				for box in boxes:
					tracker = OPENCV_OBJECT_TRACKERS[args["tracker"]]()
					trackers.add(tracker, frame, tuple(box))

				replace = input('Replace box? [y/n]')
				if replace == 'y':
					box = cv2.selectROI("Frame", frame, fromCenter = False, showCrosshair = True)
					IDs.append(box_id)
					tracker = OPENCV_OBJECT_TRACKERS[args["tracker"]]()
					trackers.add(tracker, frame, box)

				remove_object = input('Remove/reset another object? [y/n]')
				if remove_object == 'n':
					break
			except:
				break

	# if the `q` key was pressed, break from the loop
	elif key == ord("q"):
		break

# if we are using a webcam, release the pointer
if not args.get("video", False):
	vs.stop()

# otherwise, release the file pointer
else:
	vs.release()

# close all windows
cv2.destroyAllWindows()
