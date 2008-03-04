package com.google.android.game2d.api.engine;

import android.app.Activity;

import com.google.android.game2d.api.sprite.LivelySprite;

public abstract class GameEngine {
	protected Activity activity;
	protected boolean isRunning;
	
	public GameEngine(Activity activity) {
		this.activity = activity;
	}
	
	public void run() {
		initialize();
		initLoop();
	}
	
	public  void initialize() {
		isRunning = true;
	}
	
	public void initLoop() {
		long startTime = System.currentTimeMillis();
		long endTime = startTime;
		while(isRunning) {
			long elapsedTime = endTime - startTime;
			update(elapsedTime);
			startTime = endTime;
			endTime = System.currentTimeMillis();
		}
		
	}
	public abstract void update(long elapsedTime);
	public abstract LivelySprite getMainCharacter();
	
}
