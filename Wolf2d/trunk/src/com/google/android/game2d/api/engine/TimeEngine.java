package com.google.android.game2d.api.engine;

/**
 * This class is responsible for managing 
 * time events, such as computing the elapsed time 
 * between two iterations of the game main loop. 
 * 
 * This class implements the singleton pattern. Therefore, 
 * we can obtain an instance of this class by calling:
 * <code>TimeEngine.instance()</code>.
 * 
 * @author rbonifacio - rba2@cin.ufpe.br
 */
public class TimeEngine {
	
	/** 
	 * Used to guarantee that a game will be running at a 
	 * maximum of 200 frames per second --- one frame for each 
	 * five5 milliseconds 
	 */ 
	public static final long MIN_FRAME_INTERVAL = 5;
	
	/**
	 * Used to guarantee that a game will return 20 frames per second. 
	 * One frame for each 50 milliseconds.
	 */
	public static final long MAX_FRAME_INTERVAL = 50;
	
	private static TimeEngine instance;
	private long lastTime;
	private long frameTime;
	private long timeAccumulator;
	private int frameCount;
	private int framesPerSecond;
	private float frameTimeInSeconds;
	private boolean priventLowDown;
	
	/** 
	 * @return unique instance of TimeEngine
	 */
	public static TimeEngine instance() {
		if (instance == null) {
			instance = new TimeEngine();
		}
		return instance;
	}

	private TimeEngine() { 
		priventLowDown = true;
		timeAccumulator = 0;
		frameCount = 0;
	}
	
	public void initialize() {
		this.lastTime = System.currentTimeMillis();
		timeAccumulator = 0;
		frameCount = 0;
	}
	
	
	/**
	 * This method is used to compute the elapsed 
	 * time between a loop iteration. It must be called 
	 * in each iteration of main loop.
	 */
	public void update() {
		long currentTime;
		frameTime = 0;
		do {
			currentTime = System.currentTimeMillis();
			frameTime = (currentTime > lastTime) ? currentTime - lastTime : 0;
			lastTime = (currentTime >= lastTime) ? currentTime : lastTime;
		}
		while(frameTime < MIN_FRAME_INTERVAL);
		
		if(priventLowDown && frameTime > MAX_FRAME_INTERVAL) {
			frameTime = MAX_FRAME_INTERVAL;
		}
		
		timeAccumulator += frameTime;
		frameCount++;
		frameTimeInSeconds = frameTime * 0.001f;
		 
		//The frame per second rate (FPS) is considered suitable 
		//when its value is in the range [30, 200]. The block of 
		//code bellow is concerned with the FPS calculus.
		if(timeAccumulator >= 1000) {
			framesPerSecond = frameCount;
			frameCount = 0;
			timeAccumulator = 0;
		}
	}

	public long getFrameTime() {
		return frameTime;
	}

	public float getFrameTimeInSeconds() {
		return frameTimeInSeconds;
	}

	public int getFramesPerSecond() {
		return framesPerSecond;
	}
	
}
