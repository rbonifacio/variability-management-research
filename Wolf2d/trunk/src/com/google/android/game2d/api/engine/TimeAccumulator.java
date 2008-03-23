package com.google.android.game2d.api.engine;

/**
 * This class is used for controlling actions 
 * that execute at specific intervals of time.  
 * 
 * For instance, some enemies are configured for 
 * shooting at each 3 seconds. We need to instantiate 
 * one time accumulator to deal with this kind of control.
 * 
 * @author rbonifacio - rba2@cin.ufpe.br
 */
public class TimeAccumulator {
	private long timeAccumulated;
	private long timeLimit;
	
	public TimeAccumulator(long timeLimit) {
		timeAccumulated = 0;
		this.timeLimit = timeLimit;
	}
	
	public void update() {
		timeAccumulated += TimeEngine.instance().getFrameTime();
	}
	
	public boolean ended() {
		return timeAccumulated >= timeLimit;
	}
	
	public void restart() {
		timeAccumulated %= timeLimit;
	}
}
