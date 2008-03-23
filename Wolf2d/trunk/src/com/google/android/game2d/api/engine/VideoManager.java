package com.google.android.game2d.api.engine;

import android.app.Activity;

/**
 * This class is responsible for rendering 
 * all game graphics. It is a singleton, however, 
 * before calling the initialize method, it is necessary 
 * to set the application activity. 
 * 
 * @author rbonifacio - rba2[at]cin.ufpe.br
 */
public class VideoManager {
 
	private static VideoManager instance;
	private Activity activity;
	
	public static VideoManager instance() {
		if (instance == null) {
			instance = new VideoManager();
		}
		return instance;
	}
	
	/**
	 * An activity must be set before the 
	 * initialization process.
	 * 
	 * @param activity the Android application activity
	 */
	public void setActivity(Activity activity) {
		this.activity = activity;
	}
	
	/**
	 * Initialize the VideoManager.
	 * @return true if successful initialize. false otherwise
	 */
	public boolean initialize() {
		return activity != null;
	}


}
