CXXFLAGS =	-O2 -g -Wall -fmessage-length=0

OBJS =		IRR.o

LIBS =

TARGET =	IRR

$(TARGET):	$(OBJS)
	$(CXX) -o $(TARGET) $(OBJS) $(LIBS)

all:	$(TARGET)

clean:
	rm -f $(OBJS) $(TARGET)
